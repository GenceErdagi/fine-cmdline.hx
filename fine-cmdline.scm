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

; Manual string split - split by space
(define (string-split-manual s)
  (define (helper chars current result)
    (cond
      [(null? chars)
       (if (null? current)
           result
           (reverse (cons (list->string (reverse current)) result)))]
      [(char=? (car chars) #\space)
       (helper (cdr chars) '() (if (null? current)
                                    result
                                    (cons (list->string (reverse current)) result)))]
      [else
       (helper (cdr chars) (cons (car chars) current) result)]))
  (helper (string->list s) '() '()))

(define (join-strings lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings (cdr lst) sep)))))

; Build a Steel expression from command string
(define (build-command-expr cmd)
  (define parts (string-split-manual cmd))
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
   "vsplit" "hsplit" "split" "new" "vnew"
   "goto-line" "goto-char" "goto-start" "goto-end"
   "find-char" "find-char-backward"
   "replace" "replace-selection"
   "insert-newline" "insert-char" "delete" "delete-selection"
   "yank" "yank-line" "paste" "paste-before"
   "undo" "redo" "repeat"
   "select-all" "select-mode" "normal-mode" "insert-mode"
   "extend" "extend-to-line" "extend-to-char"
   "match-brackets" "wrap" "join"
   "tab-switch" "tab-next" "tab-previous"
   "window-mode" "view-mode"
   "theme" "set" "get"
   "run-shell-command" "term" "make"
   "format" "format-selection"
   "hover" "signature-help" "definition" "type-definition"
   "references" "rename" "code-action"
   "lint" "workspace-symbol" "document-symbol"
   "extend-line" "extend-char" "extend-word"
   "shell-pipeline" "pipe" "runnable"))

; Execute command using eval-string for one-to-one helix command compatibility
(define (execute-helix-command cmd)
  (when (and (string? cmd) (> (string-length cmd) 0))
    (define expr (build-command-expr cmd))
    (when (> (string-length expr) 0)
      (eval-string expr))))

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

(define (get-completions input all-commands)
  (if (string=? input "")
      all-commands
      (let* ([parts (string-split-manual input)]
             [first-word (if (null? parts) "" (car parts))]
             [has-bang? (and (> (string-length first-word) 0)
                             (char=? (string-ref first-word (- (string-length first-word) 1)) #\!))]
             [base-word (if has-bang? (substring first-word 0 (- (string-length first-word) 1)) first-word)])
        (if (string=? base-word "")
            '()
            (let ([matches (filter (lambda (cmd) (fuzzy-match? base-word cmd)) all-commands)])
              (if has-bang?
                  (map (lambda (m) (string-append m "!")) matches)
                  matches))))))

; ============================================================
; Rendering
; ============================================================

; Configuration for fine-cmdline
(define *fine-cmdline-config* 
  (hash "width" 60
        "max-completions" 8
        "offset-x" #f  ; #f = center, or specify offset
        "offset-y" #f)) ; #f = center, or specify offset

(define (fine-cmdline-config! key value)
  (set! *fine-cmdline-config* (hash-insert *fine-cmdline-config* key value)))

(define (cmdline-render state rect frame)
  (define width (area-width rect))
  (define height (area-height rect))
  
  (define input-str (text-field->string (CmdlineState-input state)))
  (define completions (unbox (CmdlineState-completions state)))
  (define completion-index (unbox (CmdlineState-completion-index state)))
  
  (define prompt ": ")
  (define prompt-len 2)
  
  ; Use native helix theme styles
  (define menu-style (theme-scope "ui.menu"))
  (define menu-bg (style->bg menu-style))
  
  ; Create styles that use the menu background (the gray color)
  (define prompt-style (~> (theme-scope "keyword") (style-bg menu-bg)))
  (define input-style (~> (theme-scope "ui.text") (style-bg menu-bg)))
  (define bg-style (~> (theme-scope "ui.background") (style-bg menu-bg)))
  
  ; Completions use the menu style directly (gray background)
  (define completion-style menu-style)
  (define selected-completion-style (theme-scope "ui.menu.selected"))
  
  ; Get config values
  (define cmd-width (hash-get *fine-cmdline-config* "width"))
  (define max-completions (hash-get *fine-cmdline-config* "max-completions"))
  (define offset-x (hash-get *fine-cmdline-config* "offset-x"))
  (define offset-y (hash-get *fine-cmdline-config* "offset-y"))
  
  ; Dynamic height based on completions
  (define num-completions (length completions))
  (define has-completions? (and (> num-completions 0) (> (string-length input-str) 0)))
  
  ; completion-height is number of completion rows to display
  (define completion-height (if has-completions?
                               (min num-completions max-completions)
                               0))
  
  ; Height: 1 input row + completion rows + 1 bottom border (top border at row 0)
  (define cmd-height (+ 3 completion-height))
  
  ; Position calculation
  (define center-x (round (/ (- width cmd-width) 2)))
  (define center-y (round (/ (- height cmd-height) 2)))
  
  ; Apply offsets if configured
  (define final-x (if offset-x (+ center-x offset-x) center-x))
  (define final-y (if offset-y (+ center-y offset-y) center-y))
  (define final-y (max final-y 1))
  
  (define cmd-area (area (+ (area-x rect) final-x)
                          (+ (area-y rect) final-y)
                          cmd-width
                          cmd-height))
  
  ; Use the gray background for the entire area
  (buffer/clear-with frame cmd-area bg-style)
  
  ; Render the block with the gray background and white borders (from ui.background foreground)
  (block/render frame
                cmd-area
                (make-block bg-style bg-style "all" "plain"))
  
  ; Add title to the top border - " Cmdline " with gray background to break the border line
  (frame-set-string! frame
                    (+ (area-x cmd-area) 1)
                    (area-y cmd-area)
                    " Cmdline "
                    bg-style)
  
  (frame-set-string! frame 
                    (+ (area-x cmd-area) 1) 
                    (+ (area-y cmd-area) 1) 
                    prompt 
                    prompt-style)
  
  ; Truncate input if it's too long
  (define max-input-len (- cmd-width prompt-len 2))
  (define display-input (if (> (string-length input-str) max-input-len)
                            (substring input-str (- (string-length input-str) max-input-len) (string-length input-str))
                            input-str))

  (frame-set-string! frame 
                    (+ (area-x cmd-area) prompt-len 1) 
                    (+ (area-y cmd-area) 1) 
                    display-input 
                    input-style)
  
  (set-position-row! (CmdlineState-cursor-position state) (+ (area-y cmd-area) 1))
  (set-position-col! (CmdlineState-cursor-position state)
                     (+ (area-x cmd-area) prompt-len 1 (string-length display-input)))
  
  ; Render completions
  (when has-completions?
    (define completion-y (+ (area-y cmd-area) 2))
    (define completion-list (slice completions 0 completion-height))
    
    (for-index (lambda (i comp)
                 (define is-selected (= i completion-index))
                 ; Truncate if longer than width - avoid overflowing borders
                 (define display-comp (if (> (string-length comp) (- cmd-width 3))
                                         (string-append (substring comp 0 (- cmd-width 6)) "...")
                                         comp))
                 (frame-set-string! frame
                                    (+ (area-x cmd-area) 1)
                                    (+ completion-y i)
                                    (string-append " " display-comp)
                                    (if is-selected 
                                        selected-completion-style 
                                        completion-style)))
               completion-list
               0)))

; ============================================================
; Event Handling
; ============================================================

(define (cmdline-cursor-handler state rect)
  (CmdlineState-cursor-position state))

(define (cmdline-event-handler state event)
  (define char (key-event-char event))
  (define input-field (CmdlineState-input state))
  (define history (CmdlineState-history state))
  (define history-index (CmdlineState-history-index state))
  (define completions-box (CmdlineState-completions state))
  (define completion-index-box (CmdlineState-completion-index state))
  (define all-commands (CmdlineState-all-commands state))
  
  (cond
    [(key-event-escape? event) 
     event-result/close]
    
    [(key-event-enter? event)
     (define input-str (text-field->string input-field))
     (when (> (string-length input-str) 0)
       (set-CmdlineState-history! state (cons input-str history))
       
       (define parts (string-split-manual input-str))
       (define cmd-name (car parts))
       (define args (cdr parts))
       
       (define completions (unbox completions-box))
       
       ; Resolve shorthand/first match if not an exact match
       (define resolved-cmd
         (if (and (not (null? completions))
                  (not (member cmd-name all-commands)))
             (car completions)
             cmd-name))
             
       (define cmd-to-execute
         (if (null? args)
             resolved-cmd
             (string-append resolved-cmd " " (join-strings args " "))))
             
       (execute-helix-command cmd-to-execute))
     event-result/close]
    
    [(key-event-backspace? event)
     (pop-char! input-field)
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box 0)
     event-result/consume]
    
    [(key-event-tab? event)
     (define completions (unbox completions-box))
     (when (> (length completions) 0)
       (define current (unbox completion-index-box))
       (define next (if (equal? (key-event-modifier event) key-modifier-shift)
                       (- current 1)
                       (+ current 1)))
       (set-box! completion-index-box 
                  (modulo next (length completions))))
     event-result/consume]
    
    [(key-event-up? event)
     (define h-index (unbox history-index))
     (when (< h-index (length history))
       (set-box! history-index (+ h-index 1))
       (define hist-cmd (list-ref history h-index))
       (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))
     event-result/consume]
    
    [(key-event-down? event)
     (define h-index (unbox history-index))
     (when (> h-index 0)
       (set-box! history-index (- h-index 1))
       (define hist-cmd (list-ref history (- h-index 1)))
       (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))
     (when (= h-index 0)
       (set-MutableTextField-text! input-field '()))
     event-result/consume]
    
    [char
     (push-char! input-field char)
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box 0)
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
                                (box 0)
                                initial-completions
                                all-cmds)
                   cmdline-render
                   (hash "handle_event" cmdline-event-handler
                         "cursor" cmdline-cursor-handler))))

(define fine-cmdline fine-cmdline/open)
