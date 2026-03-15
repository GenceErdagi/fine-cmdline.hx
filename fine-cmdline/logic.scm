(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require "fine-cmdline/utils.scm")
(require "fine-cmdline/types.scm")
(require "fine-cmdline/config.scm")

(provide push-char!
         pop-char!
         text-field->string
         execute-helix-command
         get-completions
         cmdline-render
         cmdline-event-handler
         cmdline-cursor-handler
         resolve-shorthand
         build-command-expr)

(define (push-char! field char)
  (define text (MutableTextField-text field))
  (set-MutableTextField-text! field (cons char text)))

(define (pop-char! field)
  (define text (MutableTextField-text field))
  (when (not (null? text))
    (set-MutableTextField-text! field (cdr text))))

(define (text-field->string field)
  (~> (MutableTextField-text field) reverse list->string))

(define (is-false? x) (equal? x #false))

(define (resolve-shorthand cmd shorthands)
  (if (is-false? cmd)
      #f
      (let ([resolved (hash-try-get shorthands cmd)])
        (if (is-false? resolved)
            cmd
            resolved))))

(define (build-command-expr cmd shorthands)
  (call/cc (lambda (return)
    (define clean-cmd (let ([trimmed (string-trim cmd)])
                        (if (and (> (string-length trimmed) 0) 
                                 (char=? (string-ref trimmed 0) #\:))
                            (substring trimmed 1 (string-length trimmed))
                            trimmed)))
    (when (string=? clean-cmd "") (return void))
    
    (define parts (string-split-manual clean-cmd))
    (when (null? parts) (return void))
    
    (define raw-cmd-name (car parts))
    (define args (cdr parts))
    
    (cond 
      [(string=? raw-cmd-name "!")
       (let ([args-str (if (null? args) "" (apply string-append (map (lambda (a) (string-append " \"" a "\"")) args)))])
         (return (string-append "(helix.static.run-shell-command \":\"" args-str ")")))]
      [(string=? raw-cmd-name "|")
       (let ([args-str (if (null? args) "" (apply string-append (map (lambda (a) (string-append " \"" a "\"")) args)))])
         (return (string-append "(helix.pipe" args-str ")")))]
      [else void])

    (define has-bang? (and (> (string-length raw-cmd-name) 1)
                           (char=? (string-ref raw-cmd-name (- (string-length raw-cmd-name) 1)) #\!)))
    (define base-name (if has-bang? 
                          (substring raw-cmd-name 0 (- (string-length raw-cmd-name) 1))
                          raw-cmd-name))
    
    (define resolved-base (resolve-shorthand base-name shorthands))
    (define cmd-name (if has-bang? (string-append resolved-base "!") resolved-base))
    
    (define args-str 
      (if (null? args)
          ""
          (string-append " " (apply string-append 
                                  (map (lambda (a) (string-append "\"" a "\" ")) args)))))
    
    (if (member resolved-base '("run-shell-command" "term" "make" "sh"))
        (string-append "(helix.static." cmd-name args-str ")")
        (string-append "(helix." cmd-name args-str ")")))))

(define (execute-helix-command cmd shorthands)
  (with-handler (lambda (err) (set-error! (to-string err)))
    (when (and (string? cmd) (> (string-length cmd) 0))
      (define expr (build-command-expr cmd shorthands))
      (when (> (string-length expr) 0)
        (eval-string expr)))))

; ============================================================
; Matching
; ============================================================

(define (fuzzy-score pattern text)
  (define pattern-len (string-length pattern))
  (define text-len (string-length text))
  (cond
    [(= pattern-len 0) 1]
    [(string=? pattern text) 1000]
    [(and (>= text-len pattern-len) (string=? (string-downcase pattern) (substring (string-downcase text) 0 pattern-len))) 80]
    [else 
     (let loop ([pi 0] [ti 0] [score 0] [at-boundary #t])
       (cond
         [(= pi pattern-len) score]
         [(= ti text-len) 0]
         [else
          (let ([p-char (char-downcase (string-ref pattern pi))]
                [t-char (char-downcase (string-ref text ti))])
            (if (char=? p-char t-char)
                (let ([new-score (+ score (if at-boundary 50 10))])
                  (loop (+ pi 1) (+ ti 1) new-score #f))
                (let ([is-boundary (or (char=? t-char #\-) (char=? t-char #\_) (char=? t-char #\/))])
                  (loop pi (+ ti 1) score is-boundary))))]))]))

(define (get-fuzzy-indices pattern text)
  (define pattern-len (string-length pattern))
  (define text-len (string-length text))
  (if (= pattern-len 0)
      '()
      (let loop ([pi 0] [ti 0] [indices '()])
        (cond
          [(= pi pattern-len) (reverse indices)]
          [(= ti text-len) '()]
          [else
           (let ([p-char (char-downcase (string-ref pattern pi))]
                 [t-char (char-downcase (string-ref text ti))])
             (if (char=? p-char t-char)
                 (loop (+ pi 1) (+ ti 1) (cons ti indices))
                 (loop pi (+ ti 1) indices)))]))))

(define (get-buffer-completions arg-to-complete prefix)
  (let* ([doc-ids (editor-all-documents)]
         [paths (map (lambda (id) (editor-document->path id)) doc-ids)]
         [valid-paths (filter (lambda (p) (and (not (void? p)) (not (equal? p #f)))) paths)])
    (map (lambda (p) (string-append prefix p))
         (filter (lambda (p) (> (fuzzy-score arg-to-complete p) 0)) valid-paths))))

(define (get-language-completions arg-to-complete prefix)
  (let ([languages '("rust" "scheme" "python" "javascript" "typescript" "go" "cpp" "c" "markdown" "toml" "yaml" "json" "html" "css")])
    (map (lambda (l) (string-append prefix l))
         (filter (lambda (l) (> (fuzzy-score arg-to-complete l) 0)) languages))))

(define (get-setting-completions arg-to-complete prefix)
  (let ([settings '("indent-heuristic" "atomic-save" "lsp" "search" "auto-pairs" "continue-comments" "popup-border" "cursor-shape" "whitespace" "indent-guides" "scrolloff" "scroll_lines" "mouse" "shell" "jump-label-alphabet" "line-number" "cursorline" "cursorcolumn" "middle-click-paste" "auto-completion" "auto-format" "auto-save" "text-width" "idle-timeout" "completion-timeout" "preview-completion-insert" "completion-trigger-len" "completion-replace" "auto-info" "true-color" "insert-final-newline" "color-modes" "gutters" "undercurl" "terminal" "rulers" "bufferline" "workspace-lsp-roots" "default-line-ending" "smart-tab" "rainbow-brackets")])
    (map (lambda (s) (string-append prefix s))
         (filter (lambda (s) (> (fuzzy-score arg-to-complete s) 0)) settings))))

(define (get-file-completions arg-to-complete prefix)
  (let* ([last-slash-pos (let loop ([chars (string->list arg-to-complete)] [last -1] [curr 0])
                           (cond [(null? chars) last]
                                 [(char=? (car chars) #\/) (loop (cdr chars) curr (+ curr 1))]
                                 [else (loop (cdr chars) last (+ curr 1))]))]
         [dir-to-read (cond [(= last-slash-pos -1) "."]
                            [(= last-slash-pos 0) "/"]
                            [else (substring arg-to-complete 0 last-slash-pos)])]
         [dir-prefix (if (= last-slash-pos -1) "" (substring arg-to-complete 0 (+ last-slash-pos 1)))]
         [file-p (substring arg-to-complete (+ last-slash-pos 1) (string-length arg-to-complete))]
         [entries (if (is-dir? dir-to-read) (read-dir dir-to-read) '())])
    (map (lambda (e) 
           (let* ([m (file-name e)]
                  [full-path (string-append (if (string=? dir-to-read ".") "" 
                                                (if (string-ends-with? dir-to-read "/") 
                                                    dir-to-read 
                                                    (string-append dir-to-read "/"))) m)]
                  [display-path (string-append dir-prefix m (if (is-dir? full-path) "/" ""))])
             (string-append prefix display-path)))
         (filter (lambda (e) (> (fuzzy-score file-p (file-name e)) 0)) entries))))

(define (string-ends-with? s suffix)
  (let ([slen (string-length s)]
        [sublen (string-length suffix)])
    (if (< slen sublen) #f (string=? (substring s (- slen sublen) slen) suffix))))

(define (get-argument-completions cmd last-part prefix parts shorthands)
  (let* ([resolved (resolve-shorthand cmd shorthands)]
         [is-at-end (string=? last-part "")]
         [num-full-args (if is-at-end (- (length parts) 1) (- (length parts) 2))])
    (cond
      [(member resolved '("theme"))
       (if (> num-full-args 0)
           '()
           (let ([themes (themes->list)])
             (map (lambda (t) (string-append prefix t))
                  (sort (filter (lambda (t) (> (fuzzy-score last-part t) 0)) themes)
                        (lambda (a b) (> (fuzzy-score last-part a) (fuzzy-score last-part b)))))))]
      [(member resolved '("buffer" "buffer-close" "buffer-close!" "buffer-close-all" "buffer-close-all!" "buffer-close-others" "buffer-close-others!" "buffer-next" "buffer-previous"))
       (if (> num-full-args 0) '() (get-buffer-completions last-part prefix))]
      [(member resolved '("set-language"))
       (if (> num-full-args 0) '() (get-language-completions last-part prefix))]
      [(member resolved '("set" "set-option" "toggle" "toggle-option"))
       (if (>= num-full-args 2) '() (get-setting-completions last-part prefix))]
      [(member resolved '("open" "vsplit" "hsplit" "read" "move"))
       (if (> num-full-args 0) '() (get-file-completions last-part prefix))]
      [else '()])))

(define (get-completions input all-commands-with-docs shorthands)
  (define all-commands (map car all-commands-with-docs))
  (if (string=? input "")
      all-commands
      (let* ([parts (string-split-manual input)]
             [first-word (if (null? parts) "" (car parts))]
             [has-bang? (and (> (string-length first-word) 0)
                             (char=? (string-ref first-word (- (string-length first-word) 1)) #\!))]
             [base-word (if has-bang? (substring first-word 0 (- (string-length first-word) 1)) first-word)])
        (cond 
          [(and (not (null? parts)) (or (> (length parts) 1) 
                                       (char=? (string-ref input (- (string-length input) 1)) #\space)))
           (let* ([cmd (car parts)]
                  [last-part (if (char=? (string-ref input (- (string-length input) 1)) #\space)
                                 ""
                                 (list-ref parts (- (length parts) 1)))]
                  [prefix (substring input 0 (- (string-length input) (string-length last-part)))])
             (get-argument-completions cmd last-part prefix parts shorthands))]
          [(string=? base-word "") '()]
          [else 
           (let* ([shorthand-target (resolve-shorthand base-word shorthands)]
                  [has-exact-shorthand? (and (not (equal? shorthand-target base-word)) 
                                             (member shorthand-target all-commands))]
                  [scored-matches (map (lambda (cmd) (cons cmd (fuzzy-score base-word cmd))) all-commands)]
                  [filtered-matches (filter (lambda (p) (> (cdr p) 0)) scored-matches)]
                  [sorted-matches (sort filtered-matches (lambda (a b) (> (cdr a) (cdr b))))]
                  [matches (map car sorted-matches)]
                  [final-matches (if has-exact-shorthand?
                                    (cons shorthand-target (filter (lambda (m) (not (string=? m shorthand-target))) matches))
                                    matches)])
             (if has-bang?
                 (map (lambda (m) (string-append m "!")) final-matches)
                 final-matches))]))))

; ============================================================
; Rendering Logic
; ============================================================

(define (render-highlighted-string frame x y text indices base-style highlight-style width)
  (define len (string-length text))
  (let loop ([i 0] [curr-x x] [rem-indices indices])
    (cond
      [(= i len) 
       (when (< (- curr-x x) width)
         (frame-set-string! frame curr-x y (make-string (- width (- curr-x x)) #\space) base-style))]
      [(and (not (null? rem-indices)) (= i (car rem-indices)))
       (let h-loop ([j i] [rj rem-indices])
         (if (and (< j len) (not (null? rj)) (= j (car rj)))
             (h-loop (+ j 1) (cdr rj))
             (begin
               (frame-set-string! frame curr-x y (substring text i j) highlight-style)
               (loop j (+ curr-x (- j i)) rj))))]
      [else
       (let n-loop ([j i])
         (if (or (= j len) (and (not (null? rem-indices)) (= j (car rem-indices))))
             (begin
               (frame-set-string! frame curr-x y (substring text i j) base-style)
               (loop j (+ curr-x (- j i)) rem-indices))
             (n-loop (+ j 1))))])))

(define (cmdline-render state rect frame)
  (define width (area-width rect))
  (define height (area-height rect))
  (define input-str (text-field->string (CmdlineState-input state)))
  (define completions (unbox (CmdlineState-completions state)))
  (define completion-index (unbox (CmdlineState-completion-index state)))
  (define all-commands (CmdlineState-all-commands state))
  (define shorthands (CmdlineState-shorthands state))
  
  (define config fine-cmdline-config-getter)
  (define cmd-width (config "width"))
  (define max-completions (config "max-completions"))
  (define offset-x (config "offset-x"))
  (define offset-y (config "offset-y"))
  (define anchor (config "anchor"))
  (define prompt (config "prompt-text"))
  (define prompt-len (string-length prompt))
  (define auto-execute (config "auto-execute-first"))
  
  (define bg-color (style->bg (theme-scope (config "bg-scope"))))
  (define prompt-style (~> (theme-scope "keyword") (style-bg bg-color)))
  (define input-style (~> (theme-scope "ui.text") (style-bg bg-color)))
  (define base-bg-style (~> (theme-scope "ui.background") (style-bg bg-color)))
  (define completion-style (~> (theme-scope "ui.menu") (style-bg bg-color)))
  (define fuzzy-highlight-fg (style->fg (theme-scope "special")))
  (define manual-selected-style (theme-scope "ui.menu.selected"))
  (define auto-active-style (~> completion-style (style-bg fuzzy-highlight-fg) (style-fg bg-color)))
  (define border-style (~> (theme-scope (config "border-scope")) (style-bg bg-color)))
  (define directory-style (~> (theme-scope "ui.text.directory") (style-bg bg-color)))
  
  (define parts (string-split-manual input-str))
  (define is-arg-mode (or (> (length parts) 1) (and (> (string-length input-str) 0) (char=? (string-ref input-str (- (string-length input-str) 1)) #\space))))
  
  (define num-completions (length completions))
  (define has-completions? (and (> num-completions 0) (> (string-length input-str) 0)))
  (define completion-height (if has-completions? (min num-completions max-completions) 0))
  
  (define selected-doc 
    (let ([idx (if (and has-completions? (= completion-index -1) auto-execute (not is-arg-mode)) 0 completion-index)])
      (if (and has-completions? (>= idx 0) (< idx num-completions))
          (let* ([comp-name (list-ref completions idx)] [doc-pair (assoc comp-name all-commands)])
            (if doc-pair (cdr doc-pair) #f))
          #f)))
  (define doc-height (if (not (is-false? selected-doc)) 2 0))
  (define padding (if (config "show-border") 1 0))
  (define cmd-height (+ (* padding 2) 1 completion-height doc-height))
  
  (define center-x (round (/ (- width cmd-width) 2)))
  (define reference-y (if (equal? anchor 'top) (round (/ height 5)) (round (/ (- height 3) 2))))
  (define final-x (if (not (is-false? offset-x)) (+ center-x offset-x) center-x))
  (define final-y (if (not (is-false? offset-y)) (+ reference-y offset-y) reference-y))
  (define cmd-area (area (+ (area-x rect) final-x) (+ (area-y rect) (max final-y 1)) cmd-width cmd-height))
  
  (buffer/clear-with frame cmd-area base-bg-style)
  (when (config "show-border") (block/render frame cmd-area (make-block base-bg-style border-style "all" (config "border-type"))))
  (when (and (config "show-title") (config "show-border"))
    (frame-set-string! frame (+ (area-x cmd-area) 1) (area-y cmd-area) (config "title-text") border-style))
  
  (frame-set-string! frame (+ (area-x cmd-area) padding) (+ (area-y cmd-area) padding) prompt prompt-style)
  (define max-input-len (- cmd-width prompt-len (* padding 2)))
  (define display-input (if (> (string-length input-str) max-input-len) (substring input-str (- (string-length input-str) max-input-len) (string-length input-str)) input-str))
  (frame-set-string! frame (+ (area-x cmd-area) prompt-len padding) (+ (area-y cmd-area) padding) display-input input-style)
  
  (set-position-row! (CmdlineState-cursor-position state) (+ (area-y cmd-area) padding))
  (set-position-col! (CmdlineState-cursor-position state) (+ (area-x cmd-area) prompt-len padding (string-length display-input)))
  
  (when has-completions?
    (define completion-y (+ (area-y cmd-area) padding 1))
    (define start (unbox (CmdlineState-window-start state)))
    (define completion-list (slice completions start completion-height))
    (define prefix-fill (make-string prompt-len #\space))
    (let* ([last-part (if (char=? (string-ref input-str (- (string-length input-str) 1)) #\space) "" (if (null? parts) "" (list-ref parts (- (length parts) 1))))]
           [pattern (if is-arg-mode last-part (if (null? parts) "" (car parts)))])
      (for-index (lambda (i comp)
                   (define actual-index (+ i start))
                   (define is-tab-selected (= actual-index completion-index))
                   (define is-auto-active (and (= completion-index -1) (= actual-index 0) auto-execute (not is-arg-mode)))
                   (define is-dir (and (> (string-length comp) 0) (char=? (string-ref comp (- (string-length comp) 1)) #\/)))
                   (define indices (get-fuzzy-indices pattern comp))
                   (define max-comp-len (- cmd-width (* padding 2) prompt-len))
                   (define display-comp (if (> (string-length comp) max-comp-len) (string-append (substring comp 0 (- max-comp-len 3)) "...") comp))
                   (define base-comp-style (cond [is-tab-selected manual-selected-style] [is-auto-active auto-active-style] [is-dir directory-style] [else completion-style]))
                   (define match-char-style (cond [is-tab-selected manual-selected-style] [is-auto-active auto-active-style] [else (~> base-comp-style (style-fg fuzzy-highlight-fg))]))
                   (frame-set-string! frame (+ (area-x cmd-area) padding) (+ completion-y i) prefix-fill base-comp-style)
                   (render-highlighted-string frame (+ (area-x cmd-area) padding prompt-len) (+ completion-y i) display-comp indices base-comp-style match-char-style (- cmd-width (* padding 2) prompt-len)))
                 completion-list 0))
    (when (not (is-false? selected-doc))
      (define doc-y (+ completion-y completion-height))
      (frame-set-string! frame (+ (area-x cmd-area) padding) doc-y (make-string (- cmd-width (* padding 2)) #\─) border-style)
      (define max-doc-len (- cmd-width (* padding 2) 2))
      (define display-doc (if (> (string-length selected-doc) max-doc-len) (string-append (substring selected-doc 0 (- max-doc-len 3)) "...") selected-doc))
      (frame-set-string! frame (+ (area-x cmd-area) padding 1) (+ doc-y 1) display-doc (~> (theme-scope "ui.text.info") (style-bg bg-color))))))

; ============================================================
; Event Handling Logic
; ============================================================

(define (cmdline-cursor-handler state rect) (CmdlineState-cursor-position state))

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
  (define max-completions (fine-cmdline-config-getter "max-completions"))
  (when (> (length completions) 0)
    (define current (unbox completion-index-box))
    (define next (modulo (+ current delta) (length completions)))
    (set-box! completion-index-box next)
    (define start (unbox window-start-box))
    (cond [(>= next (+ start max-completions)) (set-box! window-start-box (+ start 1))]
          [(< next start) (set-box! window-start-box next)]
          [(and (= next 0) (> current 0)) (set-box! window-start-box 0)]
          [(and (= next (- (length completions) 1)) (< current next) (< current start)) (set-box! window-start-box (max 0 (- (length completions) max-completions)))]
          [else void])
    (when update-input? (update-input-from-completion state))))

(define (cmdline-event-handler state event)
  (define char (key-event-char event))
  (define input-field (CmdlineState-input state))
  (define history (CmdlineState-history state))
  (define history-index (CmdlineState-history-index state))
  (define completions-box (CmdlineState-completions state))
  (define completion-index-box (CmdlineState-completion-index state))
  (define window-start-box (CmdlineState-window-start state))
  (define all-commands (CmdlineState-all-commands state))
  (define shorthands (CmdlineState-shorthands state))
  (define fill-on-tab (fine-cmdline-config-getter "fill-on-tab"))
  (define auto-execute (fine-cmdline-config-getter "auto-execute-first"))
  (cond
    [(key-event-escape? event) event-result/close]
    [(key-event-enter? event)
     (define input-str (text-field->string input-field))
     (when (> (string-length input-str) 0)
       (let* ([new-history (cons input-str history)]) (set-CmdlineState-history! state new-history) (save-history-to-disk new-history))
       (define completions (unbox completions-box))
       (define completion-index (unbox completion-index-box))
       (define parts (string-split-manual input-str))
       (define is-arg-mode (or (> (length parts) 1) (char=? (string-ref input-str (- (string-length input-str) 1)) #\space)))
       (define cmd-to-execute (cond [(and (not (null? completions)) (not (= completion-index -1))) (list-ref completions completion-index)]
                                   [(and (not (null? completions)) (not is-arg-mode) auto-execute) (car completions)]
                                   [else input-str]))
       (execute-helix-command cmd-to-execute shorthands))
     event-result/close]
    [(key-event-backspace? event)
     (pop-char! input-field)
     (set-box! completions-box (get-completions (text-field->string input-field) all-commands shorthands))
     (set-box! completion-index-box -1)
     (set-box! window-start-box 0)
     event-result/consume]
    [(key-event-tab? event)
     (if (equal? (key-event-modifier event) key-modifier-shift) (move-completion-cursor state -1 fill-on-tab) (move-completion-cursor state 1 fill-on-tab))
     event-result/consume]
    [(key-event-up? event)
     (define completions (unbox completions-box))
     (if (and (not (null? completions)) (> (length completions) 0))
         (move-completion-cursor state -1 fill-on-tab)
         (let ([h-index (unbox history-index)])
           (when (< h-index (length history))
             (set-box! history-index (+ h-index 1))
             (set-MutableTextField-text! input-field (reverse (string->list (list-ref history h-index)))))))
     event-result/consume]
    [(key-event-down? event)
     (define completions (unbox completions-box))
     (if (and (not (null? completions)) (> (length completions) 0))
         (move-completion-cursor state 1 fill-on-tab)
         (let ([h-index (unbox history-index)])
           (cond [(> h-index 1) (set-box! history-index (- h-index 1)) (set-MutableTextField-text! input-field (reverse (string->list (list-ref history (- h-index 2)))))]
                 [(= h-index 1) (set-box! history-index 0) (set-MutableTextField-text! input-field '())]
                 [else void])))
     event-result/consume]
    [(and (equal? char #\p) (equal? (key-event-modifier event) key-modifier-ctrl)) (move-completion-cursor state -1 fill-on-tab) event-result/consume]
    [(and (equal? char #\n) (equal? (key-event-modifier event) key-modifier-ctrl)) (move-completion-cursor state 1 fill-on-tab) event-result/consume]
    [char
     (push-char! input-field char)
     (set-box! completions-box (get-completions (text-field->string input-field) all-commands shorthands))
     (set-box! completion-index-box -1)
     (set-box! window-start-box 0)
     event-result/consume]
    [else event-result/ignore]))
