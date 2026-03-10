; fine-cmdline.scm - A fine-grained command line input for Helix
; Inspired by fine-cmdline.nvim (https://github.com/vonheikemen/fine-cmdline.nvim)

(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/ext.scm")

(provide fine-cmdline
         fine-cmdline/open
         fine-cmdline-config!)

; ============================================================
; Helper Functions
; ============================================================

(define (for-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (for-index func (cdr lst) (+ index 1)))))

; Improved string split - handles quotes and spaces
(define (string-split-manual s)
  (define (helper chars current result in-quotes)
    (cond
      [(null? chars)
       (if (null? current)
           (reverse result)
           (reverse (cons (list->string (reverse current)) result)))]
      [(char=? (car chars) #\")
       (helper (cdr chars) current result (not in-quotes))]
      [(and (char=? (car chars) #\space) (not in-quotes))
       (if (null? current)
           (helper (cdr chars) '() result #f)
           (helper (cdr chars) '() (cons (list->string (reverse current)) result) #f))]
      [else
       (helper (cdr chars) (cons (car chars) current) result in-quotes)]))
  (helper (string->list s) '() '() #f))

(define (join-strings lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings (cdr lst) sep)))))

; Build a Steel expression from command string
(define (build-command-expr cmd)
  ; Strip leading colon if present (e.g. from script input)
  (define clean-cmd (if (and (> (string-length cmd) 0) 
                             (char=? (string-ref cmd 0) #\:))
                        (substring cmd 1 (string-length cmd))
                        cmd))
  (define parts (string-split-manual clean-cmd))
  (when (null? parts) (return! void))
  (define cmd-name (car parts))
  (define args (cdr parts))
  
  ; Build expression: (helix.cmd-name "arg1" "arg2" ...)
  (define args-str 
    (if (null? args)
        ""
        (string-append " " (apply string-append 
                                (map (lambda (a) (string-append "\"" a "\" ")) args)))))
  
  (string-append "(helix." cmd-name args-str ")"))

; ============================================================
; Data Structures
; ============================================================

(struct CmdlineState 
  (input                           ; MutableTextField - user input
   history                         ; List of past commands
   history-index                   ; Box int - current position in history
   cursor-position                 ; Position - where cursor is rendered
   completion-index                ; Box int - selected completion
   window-start                    ; Box int - for scrollable list
   completions                     ; Box list - available completions
   all-commands) #:mutable)

(struct MutableTextField (text) #:mutable)

(define (push-char! field char)
  (define text (MutableTextField-text field))
  (set-MutableTextField-text! field (cons char text)))

(define (pop-char! field)
  (define text (MutableTextField-text field))
  (when (not (null? text))
    (set-MutableTextField-text! field (cdr text))))

(define (text-field->string field)
  (~> (MutableTextField-text field) reverse list->string))

; ============================================================
; Command Execution
; ============================================================

(define (get-all-commands)
  (list 
   "write" "quit" "qall" "save" "open" "buffer-close" "buffer-close-other"
   "vsplit" "hsplit" "split" "new" "vnew" "only" "next" "prev"
   "goto-line" "goto-char" "goto-start" "goto-end" "goto-next-buffer" "goto-previous-buffer"
   "find-char" "find-char-backward" "search" "search-backward" "search-next" "search-prev"
   "replace" "replace-selection" "grep" "global-search"
   "insert-newline" "insert-char" "delete" "delete-selection" "change-selection"
   "yank" "yank-line" "paste" "paste-before" "replace-with-yanked"
   "undo" "redo" "repeat"
   "select-all" "select-mode" "normal-mode" "insert-mode" "select-regex" "split-selection" "split-selection-on-newline"
   "extend" "extend-to-line" "extend-to-char" "extend-line" "extend-char" "extend-word"
   "match-brackets" "wrap" "join" "keep-lines" "remove-lines" "keep-primary-selection" "remove-primary-selection"
   "tab-switch" "tab-next" "tab-previous" "tab-close" "tab-new"
   "window-mode" "view-mode" "horizontal-split" "vertical-split"
   "theme" "set" "get" "toggle" "set-language" "reload" "reload-all" "config-reload" "config-open"
   "run-shell-command" "term" "make" "sh" "pipe" "shell-pipeline" "runnable"
   "format" "format-selection" "indent" "unindent"
   "hover" "signature-help" "definition" "type-definition" "implementation" "references" "rename" "code-action"
   "lint" "workspace-symbol" "document-symbol" "diagnostics" "workspace-diagnostics"
   "log-open" "tutor" "version" "clipboard-yank" "clipboard-paste-after" "clipboard-paste-before"
   "buffer-next" "buffer-previous" "buffer" "b" "bd" "bdelete" "blist" "buffers"
   "jump-backward" "jump-forward" "save-selection" "earlier" "later"
   "tree-sitter-scopes" "tree-sitter-highlight-name" "tree-sitter-subtree"
   "sort" "reverse" "unique"))

; Execute command using eval-string for one-to-one helix command compatibility
(define (execute-helix-command cmd)
  (with-handler (lambda (err) (set-error! (to-string err)))
    (when (and (string? cmd) (> (string-length cmd) 0))
      (define expr (build-command-expr cmd))
      (when (> (string-length expr) 0)
        (eval-string expr)))))

; ============================================================
; Completion Matching
; ============================================================

(define (fuzzy-match? pattern text)
  (define pattern-len (string-length pattern))
  (define text-len (string-length text))
  (when (> pattern-len 0)
    (let loop ([pi 0] [ti 0])
      (cond
        [(= pi pattern-len) #t]
        [(= ti text-len) #f]
        [(char=? (string-ref pattern pi) (string-ref text ti))
         (loop (+ pi 1) (+ ti 1))]
        [else (loop pi (+ ti 1))]))))

(define (string-ends-with? s suffix)
  (let ([slen (string-length s)]
        [sublen (string-length suffix)])
    (if (< slen sublen)
        #f
        (string=? (substring s (- slen sublen) slen) suffix))))

(define (is-false? x) (equal? x #false))

(define (get-buffer-completions arg-to-complete prefix)
  (let* ([doc-ids (editor-all-documents)]
         [paths (map (lambda (id) (editor-document->path id)) doc-ids)]
         ; document-path returns #false for unnamed buffers (scratch), filter them out
         [valid-paths (filter (lambda (p) (and (not (void? p)) (not (is-false? p)))) paths)])
    (map (lambda (p) (string-append prefix p))
         (filter (lambda (p) (fuzzy-match? arg-to-complete p)) valid-paths))))

(define (get-language-completions arg-to-complete prefix)
  (let ([languages '("rust" "scheme" "python" "javascript" "typescript" "go" "cpp" "c" "markdown" "toml" "yaml" "json" "html" "css")])
    (map (lambda (l) (string-append prefix l))
         (filter (lambda (l) (fuzzy-match? arg-to-complete l)) languages))))

(define (get-completions input all-commands)
  (if (string=? input "")
      all-commands
      (let* ([parts (string-split-manual input)]
             [first-word (if (null? parts) "" (car parts))]
             [has-bang? (and (> (string-length first-word) 0)
                             (char=? (string-ref first-word (- (string-length first-word) 1)) #\!))]
             [base-word (if has-bang? (substring first-word 0 (- (string-length first-word) 1)) first-word)])
        
        (cond 
          ; Argument completion
          [(and (not (null? parts)) (or (> (length parts) 1) 
                                       (char=? (string-ref input (- (string-length input) 1)) #\space)))
           (let* ([cmd (car parts)]
                  [last-part (if (char=? (string-ref input (- (string-length input) 1)) #\space)
                                 ""
                                 (list-ref parts (- (length parts) 1)))]
                  [prefix (substring input 0 (- (string-length input) (string-length last-part)))])
             
             (cond
               [(member cmd '("theme"))
                (let ([themes (themes->list)])
                  (map (lambda (t) (string-append prefix t))
                       (filter (lambda (t) (fuzzy-match? last-part t)) themes)))]
               
               [(member cmd '("buffer" "b" "bdelete" "bd"))
                (get-buffer-completions last-part prefix)]
               
               [(member cmd '("set-language"))
                (get-language-completions last-part prefix)]
               
               [(member cmd '("open" "e" "vsplit" "vs" "hsplit" "hs"))
                (let* ([last-slash-pos (let loop ([chars (string->list last-part)] [last -1] [curr 0])
                                         (cond [(null? chars) last]
                                               [(char=? (car chars) #\/) (loop (cdr chars) curr (+ curr 1))]
                                               [else (loop (cdr chars) last (+ curr 1))]))]
                       [dir-to-read (cond [(= last-slash-pos -1) "."]
                                          [(= last-slash-pos 0) "/"]
                                          [else (substring last-part 0 last-slash-pos)])]
                       [dir-prefix (if (= last-slash-pos -1) "" (substring last-part 0 (+ last-slash-pos 1)))]
                       [file-p (substring last-part (+ last-slash-pos 1) (string-length last-part))]
                       [entries (if (is-dir? dir-to-read) (read-dir dir-to-read) '())])
                  (map (lambda (e) 
                         (let* ([m (file-name e)]
                                [full-path (string-append (if (string=? dir-to-read ".") "" 
                                                              (if (string-ends-with? dir-to-read "/") 
                                                                  dir-to-read 
                                                                  (string-append dir-to-read "/"))) m)]
                                [display-path (string-append dir-prefix m (if (is-dir? full-path) "/" ""))])
                           (string-append prefix display-path)))
                       (filter (lambda (e) (fuzzy-match? file-p (file-name e))) entries)))]
               
               [else '()]))]
          
          ; Command completion
          [(string=? base-word "") '()]
          [else 
           (let ([matches (filter (lambda (cmd) (fuzzy-match? base-word cmd)) all-commands)])
             (if has-bang?
                 (map (lambda (m) (string-append m "!")) matches)
                 matches))]))))

; ============================================================
; Rendering
; ============================================================

; Configuration for fine-cmdline
(define *fine-cmdline-config* 
  (hash "width" 100
        "max-completions" 10
        "offset-x" #f  ; #f = center, or specify offset
        "offset-y" #f  ; #f = center/top, or specify offset
        "anchor" 'top  ; 'top or 'center
        "show-title" #t
        "title-text" " Cmdline "
        "prompt-text" ": "
        "show-border" #t
        "border-type" "plain" ; "plain", "rounded", "double", "thick"
        "bg-scope" "ui.menu"  ; Theme scope for background (e.g. "ui.menu", "ui.background")
        "border-scope" "ui.background" ; Theme scope for borders (inherits foreground)
        "fill-on-tab" #t      ; Fill input field when tabbing through completions
        "auto-execute-first" #t)) ; If Enter is pressed without moving cursor, execute first match

(define (fine-cmdline-config! key value)
  (set! *fine-cmdline-config* (hash-insert *fine-cmdline-config* key value)))

(define (cmdline-render state rect frame)
  (define width (area-width rect))
  (define height (area-height rect))
  
  (define input-str (text-field->string (CmdlineState-input state)))
  (define completions (unbox (CmdlineState-completions state)))
  (define completion-index (unbox (CmdlineState-completion-index state)))
  
  ; Get config values
  (define cmd-width (hash-get *fine-cmdline-config* "width"))
  (define max-completions (hash-get *fine-cmdline-config* "max-completions"))
  (define offset-x (hash-get *fine-cmdline-config* "offset-x"))
  (define offset-y (hash-get *fine-cmdline-config* "offset-y"))
  (define anchor (hash-get *fine-cmdline-config* "anchor"))
  (define show-title (hash-get *fine-cmdline-config* "show-title"))
  (define title-text (hash-get *fine-cmdline-config* "title-text"))
  (define prompt (hash-get *fine-cmdline-config* "prompt-text"))
  (define prompt-len (string-length prompt))
  (define show-border (hash-get *fine-cmdline-config* "show-border"))
  (define border-type (hash-get *fine-cmdline-config* "border-type"))
  (define bg-scope (hash-get *fine-cmdline-config* "bg-scope"))
  (define border-scope (hash-get *fine-cmdline-config* "border-scope"))
  
  ; Use native helix theme styles
  (define bg-theme-style (theme-scope bg-scope))
  (define bg-color (style->bg bg-theme-style))
  
  ; Create styles that use the configured background
  (define prompt-style (~> (theme-scope "keyword") (style-bg bg-color)))
  (define input-style (~> (theme-scope "ui.text") (style-bg bg-color)))
  (define base-bg-style (~> (theme-scope "ui.background") (style-bg bg-color)))
  
  ; Completions use the configured background
  (define completion-style (~> (theme-scope "ui.menu") (style-bg bg-color)))
  (define selected-completion-style (theme-scope "ui.menu.selected"))
  (define directory-style (~> (theme-scope "ui.text.directory") (style-bg bg-color)))
  
  ; Border style
  (define border-theme-style (theme-scope border-scope))
  (define border-style (~> border-theme-style (style-bg bg-color)))
  
  ; Dynamic height based on completions
  (define num-completions (length completions))
  (define has-completions? (and (> num-completions 0) (> (string-length input-str) 0)))
  
  ; completion-height is number of completion rows to display
  (define completion-height (if has-completions?
                               (min num-completions max-completions)
                               0))
  
  ; Border adjustments
  (define padding (if show-border 1 0))
  (define cmd-height (+ (* padding 2) 1 completion-height))
  
  ; Position calculation - Reference point is the input line
  (define center-x (round (/ (- width cmd-width) 2)))
  (define reference-y (if (equal? anchor 'top)
                          (round (/ height 5)) ; Top anchor: 20% down
                          (round (/ (- height 3) 2)))) ; Center anchor
  
  ; Apply offsets relative to the anchor
  (define final-x (if (not (is-false? offset-x)) (+ center-x offset-x) center-x))
  (define final-y (if (not (is-false? offset-y)) (+ reference-y offset-y) reference-y))
  (define final-y (max final-y 1))
  
  (define cmd-area (area (+ (area-x rect) final-x)
                          (+ (area-y rect) final-y)
                          cmd-width
                          cmd-height))
  
  ; Use the background for the entire area
  (buffer/clear-with frame cmd-area base-bg-style)
  
  ; Render the block/border
  (when show-border
    (block/render frame
                  cmd-area
                  (make-block base-bg-style border-style "all" border-type)))
  
  ; Optional title (only if borders are shown)
  (when (and show-title show-border)
    (frame-set-string! frame
                      (+ (area-x cmd-area) 1)
                      (area-y cmd-area)
                      title-text
                      border-style))
  
  (frame-set-string! frame 
                    (+ (area-x cmd-area) padding) 
                    (+ (area-y cmd-area) padding) 
                    prompt 
                    prompt-style)
  
  ; Truncate input if it's too long
  (define max-input-len (- cmd-width prompt-len (* padding 2)))
  (define display-input (if (> (string-length input-str) max-input-len)
                            (substring input-str (- (string-length input-str) max-input-len) (string-length input-str))
                            input-str))

  (frame-set-string! frame 
                    (+ (area-x cmd-area) prompt-len padding) 
                    (+ (area-y cmd-area) padding) 
                    display-input 
                    input-style)
  
  (set-position-row! (CmdlineState-cursor-position state) (+ (area-y cmd-area) padding))
  (set-position-col! (CmdlineState-cursor-position state)
                     (+ (area-x cmd-area) prompt-len padding (string-length display-input)))
  
  ; Render completions
  (when has-completions?
    (define completion-y (+ (area-y cmd-area) padding 1))
    (define start (unbox (CmdlineState-window-start state)))
    (define completion-list (slice completions start completion-height))
    
    (for-index (lambda (i comp)
                 (define actual-index (+ i start))
                 (define is-selected (= actual-index completion-index))
                 ; Check if it's a directory (ends with /) for syntax highlighting
                 (define is-dir (and (> (string-length comp) 0) 
                                     (char=? (string-ref comp (- (string-length comp) 1)) #\/)))
                 
                 ; Truncate if longer than width - avoid overflowing borders
                 (define max-comp-len (- cmd-width (* padding 2) 1))
                 (define display-comp (if (> (string-length comp) max-comp-len)
                                         (string-append (substring comp 0 (- max-comp-len 3)) "...")
                                         comp))
                 
                 (define final-style (cond [is-selected selected-completion-style]
                                          [is-dir directory-style]
                                          [else completion-style]))

                 (frame-set-string! frame
                                    (+ (area-x cmd-area) padding)
                                    (+ completion-y i)
                                    (string-append " " display-comp)
                                    final-style))
               completion-list
               0)))

; ============================================================
; Event Handling
; ============================================================

(define (cmdline-cursor-handler state rect)
  (CmdlineState-cursor-position state))

(define (update-input-from-completion state)
  (define completions (unbox (CmdlineState-completions state)))
  (define index (unbox (CmdlineState-completion-index state)))
  (when (and (not (null? completions)) (>= index 0) (< index (length completions)))
    (define comp (list-ref completions index))
    (set-MutableTextField-text! (CmdlineState-input state) (reverse (string->list comp)))))

(define (move-completion-cursor state delta update-input?)
  (define completion-index-box (CmdlineState-completion-index state))
  (define window-start-box (CmdlineState-window-start state))
  (define completions (unbox (CmdlineState-completions state)))
  (define max-completions (hash-get *fine-cmdline-config* "max-completions"))
  
  (when (> (length completions) 0)
    (define current (unbox completion-index-box))
    ; If nothing selected, start at -1 so delta 1 goes to 0
    (define next (modulo (+ current delta) (length completions)))
    (set-box! completion-index-box next)
    
    ; Adjust window-start
    (define start (unbox window-start-box))
    
    (cond
      ; Scrolling down past bottom
      [(>= next (+ start max-completions))
       (set-box! window-start-box (+ start 1))]
      ; Scrolling up past top
      [(< next start)
       (set-box! window-start-box next)]
      ; Wrapping from bottom to top
      [(and (= next 0) (> current 0))
       (set-box! window-start-box 0)]
      ; Wrapping from top to bottom
      [(and (= next (- (length completions) 1)) (< current next) (< current start))
       (set-box! window-start-box (max 0 (- (length completions) max-completions)))]
      [else void])
    
    (when update-input?
      (update-input-from-completion state))))

(define (cmdline-event-handler state event)
  (define char (key-event-char event))
  (define input-field (CmdlineState-input state))
  (define history (CmdlineState-history state))
  (define history-index (CmdlineState-history-index state))
  (define completions-box (CmdlineState-completions state))
  (define completion-index-box (CmdlineState-completion-index state))
  (define window-start-box (CmdlineState-window-start state))
  (define all-commands (CmdlineState-all-commands state))
  
  ; Config options
  (define fill-on-tab (hash-get *fine-cmdline-config* "fill-on-tab"))
  (define auto-execute (hash-get *fine-cmdline-config* "auto-execute-first"))
  
  (cond
    [(key-event-escape? event) 
     event-result/close]
    
    [(key-event-enter? event)
     (define input-str (text-field->string input-field))
     (when (> (string-length input-str) 0)
       (set-CmdlineState-history! state (cons input-str history))
       
       (define completions (unbox completions-box))
       (define completion-index (unbox completion-index-box))
       
       ; First match logic
       (define cmd-to-execute
         (if (and (not (null? completions)) 
                  (or (not (= completion-index -1)) auto-execute))
             (list-ref completions (if (= completion-index -1) 0 completion-index))
             input-str))
             
       (execute-helix-command cmd-to-execute))
     event-result/close]
    
    [(key-event-backspace? event)
     (pop-char! input-field)
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box -1)
     (set-box! window-start-box 0)
     event-result/consume]
    
    [(key-event-tab? event)
     (if (equal? (key-event-modifier event) key-modifier-shift)
         (move-completion-cursor state -1 fill-on-tab)
         (move-completion-cursor state 1 fill-on-tab))
     event-result/consume]
    
    [(key-event-up? event)
     (define completions (unbox completions-box))
     (if (and (not (null? completions)) (> (length completions) 0))
         (move-completion-cursor state -1 fill-on-tab)
         (let ([h-index (unbox history-index)])
           (when (< h-index (length history))
             (set-box! history-index (+ h-index 1))
             (define hist-cmd (list-ref history h-index))
             (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))))
     event-result/consume]
    
    [(key-event-down? event)
     (define completions (unbox completions-box))
     (if (and (not (null? completions)) (> (length completions) 0))
         (move-completion-cursor state 1 fill-on-tab)
         (let ([h-index (unbox history-index)])
           (when (> h-index 0)
             (set-box! history-index (- h-index 1))
             (define hist-cmd (list-ref history (- h-index 1)))
             (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))
           (when (= h-index 0)
             (set-MutableTextField-text! input-field '()))))
     event-result/consume]
    
    ; Add C-p and C-n support for scrolling
    [(and (equal? char #\p) (equal? (key-event-modifier event) key-modifier-ctrl))
     (move-completion-cursor state -1 fill-on-tab)
     event-result/consume]
    [(and (equal? char #\n) (equal? (key-event-modifier event) key-modifier-ctrl))
     (move-completion-cursor state 1 fill-on-tab)
     event-result/consume]
    
    [char
     (push-char! input-field char)
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box -1)
     (set-box! window-start-box 0)
     event-result/consume]
    
    [(mouse-event? event) 
     event-result/ignore]
    
    [else 
     event-result/ignore]))

; ============================================================
; Public API
; ============================================================

(define (fine-cmdline/open)
  (define input-field (MutableTextField '()))
  (define all-cmds (get-all-commands))
  (define initial-completions (box all-cmds))
  
  (push-component! 
   (new-component! "fine-cmdline"
                   (CmdlineState input-field
                                '()
                                (box 0)
                                (position 0 0)
                                (box -1) ; Initialize to -1
                                (box 0)
                                initial-completions
                                all-cmds)
                   cmdline-render
                   (hash "handle_event" cmdline-event-handler
                         "cursor" cmdline-cursor-handler))))

(define fine-cmdline fine-cmdline/open)
