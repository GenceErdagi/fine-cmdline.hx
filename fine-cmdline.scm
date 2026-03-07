; fine-cmdline.scm - A fine-grained command line input for Helix
; Inspired by fine-cmdline.nvim (https://github.com/vonheikemen/fine-cmdline.nvim)

(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")

(provide fine-cmdline
         fine-cmdline/open)

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
   all-commands) #:mutable)      ; List of all available commands

; Mutable text field similar to picker.scm
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

; Get list of available commands from helix
(define (get-all-commands)
  (require-builtin helix/core/keymaps as helix.keymaps.)
  ; This is a basic list - in a full implementation, we'd get this from helix
  (list 
   "write" "quit" "save" "open" "buffer-close" "buffer-close-other"
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
   "window-mode" "view-mode" "追"
   "theme" "set" "get"
   "run-shell-command" "term" "make"
   "format" "format-selection"
   "hover" "signature-help" "definition" "type-definition"
   "references" "rename" "code-action"
   "lint" "workspace-symbol" "document-symbol"
   "extend-line" "extend-char" "extend-word"
   "shell-pipeline" "pipe" "runnable"
   ))

; Execute a command string
(define (execute-cmdline cmd)
  (when (and (string? cmd) (> (string-length cmd) 0))
    ; Add to history (prepend)
    ; For now, execute via run-shell-command with : prefix
    (helix.run-shell-command ":" cmd)))

; ============================================================
; Completion Matching
; ============================================================

; Simple fuzzy match - returns #t if pattern matches text
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

; Get completions for current input
(define (get-completions input all-commands)
  (if (string=? input "")
      all-commands
      (filter (lambda (cmd) (fuzzy-match? input cmd)) all-commands)))

; ============================================================
; Rendering
; ============================================================

(define (cmdline-render state rect frame)
  ; Calculate dimensions
  (define width (area-width rect))
  (define height (area-height rect))
  
  ; Input text
  (define input-str (text-field->string (CmdlineState-input state)))
  
  ; Get completions
  (define completions (unbox (CmdlineState-completions state)))
  (define completion-index (unbox (CmdlineState-completion-index state)))
  
  ; Prompt
  (define prompt ": ")
  (define prompt-len 2)
  
  ; Calculate input area
  (define input-area-width (- width 4))
  
  ; Styles
  (define prompt-style (theme-scope "keyword"))
  (define input-style (theme-scope "ui.text"))
  (define completion-style (theme-scope "ui.text"))
  (define selected-completion-style 
    (~> (style)
        (style-bg (style->bg (theme-scope "ui.selection")))
        (style-fg (style->fg (theme-scope "ui.text"))))
  
  ; Clear the area
  (buffer/clear-with frame rect (theme-scope "ui.background"))
  
  ; Draw border
  (block/render frame
                rect
                (make-block (theme-scope "ui.background") 
                           (theme-scope "ui.background") 
                           "all" "rounded"))
  
  ; Draw prompt
  (frame-set-string! frame 
                    (+ (area-x rect) 1) 
                    (+ (area-y rect) 1) 
                    prompt 
                    prompt-style)
  
  ; Draw input
  (frame-set-string! frame 
                    (+ (area-x rect) prompt-len 1) 
                    (+ (area-y rect) 1) 
                    input-str 
                    input-style)
  
  ; Update cursor position
  (set-position-row! (CmdlineState-cursor-position state) (+ (area-y rect) 1))
  (set-position-col! (CmdlineState-cursor-position state)
                     (+ (area-x rect) prompt-len 1 (string-length input-str)))
  
  ; Draw completions (if any and input is not empty)
  (when (and (> (length completions 0)) (> (string-length input) 0))
    (define max-completions 5)
    (define completion-y (+ (area-y rect) 3))
    
    (for-each (lambda (i comp)
                 (when (< i max-completions)
                   (define is-selected (= i completion-index))
                   (frame-set-string! frame
                                      (+ (area-x rect) 2)
                                      (+ completion-y i)
                                      (string-append "  " comp)
                                      (if is-selected 
                                          selected-completion-style 
                                          completion-style))))
               (range 0 (length completions)))))

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
    ; Escape - close
    [(key-event-escape? event) 
     event-result/close]
    
    ; Enter - execute command
    [(key-event-enter? event)
     (define cmd (text-field->string input-field))
     (when (> (string-length cmd) 0)
       ; Add to history
       (set-CmdlineState-history! state (cons cmd history))
       (execute-cmdline cmd))
     event-result/close]
    
    ; Backspace - delete character
    [(key-event-backspace? event)
     (pop-char! input-field)
     ; Update completions
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box 0)
     event-result/consume]
    
    ; Tab - cycle completions
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
    
    ; Up arrow - history navigation
    [(key-event-up? event)
     (define h-index (unbox history-index))
     (when (< h-index (length history))
       (set-box! history-index (+ h-index 1))
       (define hist-cmd (list-ref history (- h-index 1)))
       (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))
     event-result/consume]
    
    ; Down arrow - history navigation
    [(key-event-down? event)
     (define h-index (unbox history-index))
     (when (> h-index 0)
       (set-box! history-index (- h-index 1))
       (define hist-cmd (list-ref history h-index))
       (set-MutableTextField-text! input-field (reverse (string->list hist-cmd))))
     (when (= h-index 0)
       (set-MutableTextField-text! input-field '()))
     event-result/consume]
    
    ; Regular character input
    [char
     (push-char! input-field char)
     ; Update completions
     (set-box! completions-box 
                (get-completions (text-field->string input-field) all-commands))
     (set-box! completion-index-box 0)
     event-result/consume]
    
    ; Mouse events - ignore for now
    [(mouse-event? event) 
     event-result/ignore]
    
    ; Default - ignore but don't close
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
                                '()                 ; history
                                (box 0)             ; history-index
                                (position 0 0)      ; cursor position
                                (box 0)             ; completion-index
                                initial-completions ; completions
                                all-cmds)           ; all commands
                   cmdline-render
                   (hash "handle_event" cmdline-event-handler
                         "cursor" cmdline-cursor-handler))))

; Backwards compatibility alias
(define fine-cmdline fine-cmdline/open)
