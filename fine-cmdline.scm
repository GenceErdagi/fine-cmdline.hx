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

(define (get-history-file-path)
  (let* ([init-path (helix.static.get-init-scm-path)]
         [dir (if (string=? init-path "") "" (parent-name init-path))])
    (if (string=? dir "")
        "fine-cmdline-history.scm"
        (string-append dir "/fine-cmdline-history.scm"))))

(define (load-history-from-disk)
  (let ([path (get-history-file-path)])
    (if (path-exists? path)
        (with-handler (lambda (err) '())
          (let ([content (read-port-to-string (open-input-file path))])
            (if (string=? content "")
                '()
                (eval-string content))))
        '())))

(define (save-history-to-disk history)
  (let ([path (get-history-file-path)])
    (with-handler (lambda (err) (set-error! (string-append "Failed to save history: " (to-string err))))
      (when (path-exists? path)
        (delete-file! path))
      (let ([port (open-output-file path)])
        (display "'(" port)
        (for-each (lambda (h) 
                    (write h port)
                    (display " " port)) 
                  history)
        (display ")" port)
        (close-output-port port)))))

(define (for-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (for-index func (cdr lst) (+ index 1)))))

(define (string-trim s)
  (define (trim-left chars)
    (cond [(null? chars) '()]
          [(char-whitespace? (car chars)) (trim-left (cdr chars))]
          [else chars]))
  (define (trim-right chars)
    (reverse (trim-left (reverse chars))))
  (list->string (trim-right (trim-left (string->list s)))))

; Improved string split - handles quotes, spaces, and escaping
(define (string-split-manual s)
  (define (helper chars current result in-quotes escaped)
    (cond
      [(null? chars)
       (if (null? current)
           (reverse result)
           (reverse (cons (list->string (reverse current)) result)))]
      [escaped
       (helper (cdr chars) (cons (car chars) current) result in-quotes #f)]
      [(char=? (car chars) #\\)
       (helper (cdr chars) current result in-quotes #t)]
      [(char=? (car chars) #\")
       (helper (cdr chars) current result (not in-quotes) #f)]
      [(and (char=? (car chars) #\space) (not in-quotes))
       (if (null? current)
           (helper (cdr chars) '() result #f #f)
           (helper (cdr chars) '() (cons (list->string (reverse current)) result) #f #f))]
      [else
       (helper (cdr chars) (cons (car chars) current) result in-quotes #f)]))
  (helper (string->list (string-trim s)) '() '() #f #f))

(define (join-strings lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings (cdr lst) sep)))))

; Build a Steel expression from command string
(define *helix-shorthands*
  (hash "!" "run-shell-command"
        "bc" "buffer-close"
        "bc!" "buffer-close!"
        "bca" "buffer-close-all"
        "bca!" "buffer-close-all!"
        "bclose" "buffer-close"
        "bclose!" "buffer-close!"
        "bcloseall" "buffer-close-all"
        "bcloseall!" "buffer-close-all!"
        "bcloseother" "buffer-close-others"
        "bcloseother!" "buffer-close-others!"
        "bco" "buffer-close-others"
        "bco!" "buffer-close-others!"
        "bn" "buffer-next"
        "bnext" "buffer-next"
        "bp" "buffer-previous"
        "bprev" "buffer-previous"
        "cd" "change-current-directory"
        "char" "character-info"
        "cq" "cquit"
        "cq!" "cquit!"
        "dbg" "debug-start"
        "dbg-tcp" "debug-remote"
        "diffg" "reset-diff-change"
        "diffget" "reset-diff-change"
        "e" "open"
        "ear" "earlier"
        "edit" "open"
        "fmt" "format"
        "g" "goto"
        "get" "get-option"
        "hnew" "hsplit-new"
        "hs" "hsplit"
        "lang" "set-language"
        "lat" "later"
        "mv" "move"
        "mv!" "move!"
        "n" "new"
        "o" "open"
        "pwd" "show-directory"
        "q" "quit"
        "q!" "quit!"
        "qa" "quit-all"
        "qa!" "quit-all!"
        "r" "read"
        "rl" "reload"
        "rla" "reload-all"
        "set" "set-option"
        "sh" "run-shell-command"
        "sp" "hsplit"
        "toggle" "toggle-option"
        "ts-subtree" "tree-sitter-subtree"
        "u" "update"
        "vnew" "vsplit-new"
        "vs" "vsplit"
        "w" "write"
        "w!" "write!"
        "wa" "write-all"
        "wa!" "write-all!"
        "wbc" "write-buffer-close"
        "wbc!" "write-buffer-close!"
        "wq" "write-quit"
        "wq!" "write-quit!"
        "wqa" "write-quit-all"
        "wqa!" "write-quit-all!"
        "x" "exit"
        "x!" "exit!"
        "xa" "write-quit-all"
        "xa!" "write-quit-all!"
        "xit" "exit"
        "xit!" "exit!"
        "|" "pipe"))

(define (is-false? x) (equal? x #false))

(define (resolve-shorthand cmd)
  (if (is-false? cmd)
      #f
      (let ([resolved (hash-try-get *helix-shorthands* cmd)])
        (if (is-false? resolved)
            cmd
            resolved))))

(define (build-command-expr cmd)
  (call/cc (lambda (return)
    ; Strip leading colon if present (e.g. from script input)
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
    
    ; Special cases for ! and | which are aliases for shell commands
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
    
    (define resolved-base (resolve-shorthand base-name))
    (define cmd-name (if has-bang? (string-append resolved-base "!") resolved-base))
    
    ; Build expression: (helix.cmd-name "arg1" "arg2" ...)
    (define args-str 
      (if (null? args)
          ""
          (string-append " " (apply string-append 
                                  (map (lambda (a) (string-append "\"" a "\" ")) args)))))
    
    ; Try both helix. and helix.static. prefixes
    (if (member resolved-base '("run-shell-command" "term" "make" "sh"))
        (string-append "(helix.static." cmd-name args-str ")")
        (string-append "(helix." cmd-name args-str ")")))))

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
  '(
   ("add_newline_above" . "Add newline above.")
   ("add_newline_below" . "Add newline below.")
   ("align_selections" . "Align selections.")
   ("align_view_bottom" . "Align view to bottom.")
   ("align_view_center" . "Align view to center.")
   ("align_view_middle" . "Align view to middle.")
   ("align_view_top" . "Align view to top.")
   ("append-output" . "Run shell command, appending output after each selection.")
   ("append_char_interactive" . "Append char interactively.")
   ("append_mode" . "Switch to append mode.")
   ("buffer-close" . "Close the current buffer.")
   ("buffer-close!" . "Close the current buffer forcefully, ignoring unsaved changes.")
   ("buffer-close-all" . "Close all buffers without quitting.")
   ("buffer-close-all!" . "Force close all buffers ignoring unsaved changes without quitting.")
   ("buffer-close-others" . "Close all buffers but the currently focused one.")
   ("buffer-close-others!" . "Force close all buffers but the currently focused one.")
   ("buffer-next" . "Goto next buffer.")
   ("buffer-previous" . "Goto previous buffer.")
   ("buffer_picker" . "Open buffer picker.")
   ("change-current-directory" . "Change the current working directory.")
   ("character-info" . "Get info about the character under the primary cursor.")
   ("clear-register" . "Clear given register. If no argument is provided, clear all registers.")
   ("clipboard-paste-after" . "Paste system clipboard after selections.")
   ("clipboard-paste-before" . "Paste system clipboard before selections.")
   ("clipboard-paste-replace" . "Replace selections with content of system clipboard.")
   ("clipboard-yank" . "Yank main selection into system clipboard.")
   ("clipboard-yank-join" . "Yank joined selections into system clipboard. A separator can be provided as first argument. Default value is newline.")
   ("code_action" . "Open code actions.")
   ("collapse_selection" . "Collapse selection.")
   ("command_mode" . "Switch to command mode.")
   ("command_palette" . "Open command palette.")
   ("commit_undo_checkpoint" . "Commit undo checkpoint.")
   ("completion" . "Open completions.")
   ("config-open" . "Open the user config.toml file.")
   ("config-open-workspace" . "Open the workspace config.toml file.")
   ("config-reload" . "Refresh user config.")
   ("cquit" . "Quit with exit code (default 1). Accepts an optional integer exit code (:cq 2).")
   ("cquit!" . "Force quit with exit code (default 1) ignoring unsaved changes. Accepts an optional integer exit code (:cq! 2).")
   ("debug-eval" . "Evaluate expression in current_selection debug context.")
   ("debug-remote" . "Connect to a debug adapter by TCP address and start a debugging session from a given template with given parameters.")
   ("debug-start" . "Start a debug session from a given template with given parameters.")
   ("decrement" . "Decrement under cursor.")
   ("delete_char_backward" . "Delete char backward.")
   ("delete_char_forward" . "Delete char forward.")
   ("delete_selection" . "Delete selection.")
   ("delete_selection_noyank" . "Delete selection without yanking.")
   ("delete_word_backward" . "Delete word backward.")
   ("delete_word_forward" . "Delete word forward.")
   ("diagnostics_picker" . "Open diagnostics picker.")
   ("echo" . "Prints the given arguments to the statusline.")
   ("encoding" . "Set encoding. Based on `https://encoding.spec.whatwg.org`.")
   ("enqueue-expression-in-engine" . "Enqueue expression in engine.")
   ("ensure_selections_forward" . "Ensure selections are forward.")
   ("exit" . "Write changes to disk if the buffer is modified and then quit. Accepts an optional path (:exit some/path.txt).")
   ("exit!" . "Force write changes to disk, creating necessary subdirectories, if the buffer is modified and then quit. Accepts an optional path (:exit! some/path.txt).")
   ("exit_select_mode" . "Exit select mode.")
   ("expand_selection" . "Expand selection.")
   ("extend_char_left" . "Extend char left.")
   ("extend_char_right" . "Extend char right.")
   ("extend_line" . "Extend line.")
   ("extend_line_above" . "Extend line above.")
   ("extend_line_below" . "Extend line below.")
   ("extend_line_down" . "Extend line down.")
   ("extend_line_up" . "Extend line up.")
   ("extend_next_char" . "Extend next char.")
   ("extend_next_long_word_end" . "Extend next long word end.")
   ("extend_next_long_word_start" . "Extend next long word start.")
   ("extend_next_sub_word_end" . "Extend next sub word end.")
   ("extend_next_sub_word_start" . "Extend next sub word start.")
   ("extend_next_word_end" . "Extend next word end.")
   ("extend_next_word_start" . "Extend next word start.")
   ("extend_parent_node_end" . "Extend parent node end.")
   ("extend_parent_node_start" . "Extend parent node start.")
   ("extend_prev_char" . "Extend prev char.")
   ("extend_prev_long_word_end" . "Extend prev long word end.")
   ("extend_prev_long_word_start" . "Extend prev long word start.")
   ("extend_prev_sub_word_end" . "Extend prev sub word end.")
   ("extend_prev_sub_word_start" . "Extend prev sub word start.")
   ("extend_prev_word_end" . "Extend prev word end.")
   ("extend_prev_word_start" . "Extend prev word start.")
   ("extend_search_next" . "Extend search next.")
   ("extend_search_prev" . "Extend search prev.")
   ("extend_till_char" . "Extend till char.")
   ("extend_till_prev_char" . "Extend till prev char.")
   ("extend_to_column" . "Extend to column.")
   ("extend_to_file_end" . "Extend to file end.")
   ("extend_to_file_start" . "Extend to file start.")
   ("extend_to_first_nonwhitespace" . "Extend to first non-whitespace.")
   ("extend_to_last_line" . "Extend to last line.")
   ("extend_to_line_bounds" . "Extend to line bounds.")
   ("extend_to_line_end" . "Extend to line end.")
   ("extend_to_line_end_newline" . "Extend to line end with newline.")
   ("extend_to_line_start" . "Extend to line start.")
   ("extend_to_word" . "Extend to word.")
   ("extend_visual_line_down" . "Extend visual line down.")
   ("extend_visual_line_up" . "Extend visual line up.")
   ("file_explorer" . "Open file explorer.")
   ("file_explorer_in_current_buffer_directory" . "Open file explorer in current buffer directory.")
   ("file_explorer_in_current_directory" . "Open file explorer in current directory.")
   ("file_picker" . "Open file picker.")
   ("file_picker_in_current_buffer_directory" . "Open file picker in current buffer directory.")
   ("file_picker_in_current_directory" . "Open file picker in current directory.")
   ("find_next_char" . "Find next char.")
   ("find_prev_char" . "Find prev char.")
   ("find_till_char" . "Find till char.")
   ("flip_selections" . "Flip selections.")
   ("format" . "Format the file using an external formatter or language server.")
   ("format_selections" . "Format selections.")
   ("get-current-column-number" . "Get current column number.")
   ("get-current-line-character" . "Get current line character.")
   ("get-current-line-number" . "Get current line number.")
   ("get-helix-cwd" . "Get Helix current working directory.")
   ("get-helix-scm-path" . "Get Helix SCM path.")
   ("get-init-scm-path" . "Get Init SCM path.")
   ("get-option" . "Get the current value of a config option.")
   ("global_search" . "Open global search.")
   ("goto" . "Goto line number.")
   ("goto-column" . "Goto column.")
   ("goto-line" . "Goto line.")
   ("goto_column" . "Goto column.")
   ("goto_declaration" . "Goto declaration.")
   ("goto_definition" . "Goto definition.")
   ("goto_file" . "Goto file.")
   ("goto_file_end" . "Goto file end.")
   ("goto_file_hsplit" . "Goto file in horizontal split.")
   ("goto_file_start" . "Goto file start.")
   ("goto_file_vsplit" . "Goto file in vertical split.")
   ("goto_first_change" . "Goto first change.")
   ("goto_first_diag" . "Goto first diagnostic.")
   ("goto_first_nonwhitespace" . "Goto first non-whitespace.")
   ("goto_implementation" . "Goto implementation.")
   ("goto_last_accessed_file" . "Goto last accessed file.")
   ("goto_last_change" . "Goto last change.")
   ("goto_last_diag" . "Goto last diagnostic.")
   ("goto_last_line" . "Goto last line.")
   ("goto_last_modification" . "Goto last modification.")
   ("goto_last_modified_file" . "Goto last modified file.")
   ("goto_line" . "Goto line.")
   ("goto_line_end" . "Goto line end.")
   ("goto_line_end_newline" . "Goto line end with newline.")
   ("goto_line_start" . "Goto line start.")
   ("goto_next_buffer" . "Goto next buffer.")
   ("goto_next_change" . "Goto next change.")
   ("goto_next_class" . "Goto next class.")
   ("goto_next_comment" . "Goto next comment.")
   ("goto_next_diag" . "Goto next diagnostic.")
   ("goto_next_entry" . "Goto next entry.")
   ("goto_next_function" . "Goto next function.")
   ("goto_next_paragraph" . "Goto next paragraph.")
   ("goto_next_parameter" . "Goto next parameter.")
   ("goto_next_tabstop" . "Goto next tabstop.")
   ("goto_next_test" . "Goto next test.")
   ("goto_next_xml_element" . "Goto next XML element.")
   ("goto_prev_change" . "Goto prev change.")
   ("goto_prev_class" . "Goto prev class.")
   ("goto_prev_comment" . "Goto prev comment.")
   ("goto_prev_diag" . "Goto prev diagnostic.")
   ("goto_prev_entry" . "Goto prev entry.")
   ("goto_prev_function" . "Goto prev function.")
   ("goto_prev_paragraph" . "Goto prev paragraph.")
   ("goto_prev_parameter" . "Goto prev parameter.")
   ("goto_prev_tabstop" . "Goto prev tabstop.")
   ("goto_prev_test" . "Goto prev test.")
   ("goto_prev_xml_element" . "Goto prev XML element.")
   ("goto_previous_buffer" . "Goto previous buffer.")
   ("goto_reference" . "Goto reference.")
   ("goto_type_definition" . "Goto type definition.")
   ("goto_window_bottom" . "Goto window bottom.")
   ("goto_window_center" . "Goto window center.")
   ("goto_window_top" . "Goto window top.")
   ("goto_word" . "Goto word.")
   ("half_page_down" . "Half page down.")
   ("half_page_up" . "Half page up.")
   ("hover" . "Open hover info.")
   ("hsplit" . "Open the file in a horizontal split.")
   ("hsplit-new" . "Open a scratch buffer in a horizontal split.")
   ("hsplit_new" . "Open a scratch buffer in a horizontal split.")
   ("increment" . "Increment under cursor.")
   ("indent" . "Indent selection.")
   ("indent-style" . "Set the indentation style for editing. ('t' for tabs or 1-16 for number of spaces.)")
   ("insert-output" . "Run shell command, inserting output before each selection.")
   ("insert_at_line_end" . "Insert at line end.")
   ("insert_at_line_start" . "Insert at line start.")
   ("insert_char" . "Insert char.")
   ("insert_char_interactive" . "Insert char interactively.")
   ("insert_mode" . "Switch to insert mode.")
   ("insert_newline" . "Insert newline.")
   ("insert_register" . "Insert register.")
   ("insert_string" . "Insert string.")
   ("insert_tab" . "Insert tab.")
   ("join_selections" . "Join selections.")
   ("join_selections_space" . "Join selections with space.")
   ("jump_backward" . "Jump backward in jump list.")
   ("jump_forward" . "Jump forward in jump list.")
   ("jump_view_down" . "Jump view down.")
   ("jump_view_left" . "Jump view left.")
   ("jump_view_right" . "Jump view right.")
   ("jump_view_up" . "Jump view up.")
   ("jumplist_picker" . "Open jump list picker.")
   ("keep_primary_selection" . "Keep primary selection.")
   ("keep_selections" . "Keep selections.")
   ("kill_to_line_end" . "Kill to line end.")
   ("kill_to_line_start" . "Kill to line start.")
   ("last_picker" . "Open last picker.")
   ("line-ending" . "Set the document's default line ending. Options: crlf, lf.")
   ("load-buffer!" . "Load current buffer.")
   ("log-open" . "Open the helix log file.")
   ("lsp-restart" . "Restarts the given language servers, or all language servers that are used by the current file if no arguments are supplied")
   ("lsp-stop" . "Stops the given language servers, or all language servers that are used by the current file if no arguments are supplied")
   ("lsp-workspace-command" . "Open workspace command picker")
   ("lsp_or_syntax_symbol_picker" . "Open LSP or syntax symbol picker.")
   ("lsp_or_syntax_workspace_symbol_picker" . "Open LSP or syntax workspace symbol picker.")
   ("make_search_word_bounded" . "Make search word bounded.")
   ("match_brackets" . "Match brackets.")
   ("merge_consecutive_selections" . "Merge consecutive selections.")
   ("merge_selections" . "Merge selections.")
   ("move" . "Move the current buffer and its corresponding file to a different path")
   ("move!" . "Move the current buffer and its corresponding file to a different path creating necessary subdirectories")
   ("new" . "Create a new scratch buffer.")
   ("no_op" . "No operation.")
   ("noop" . "Does nothing.")
   ("normal_mode" . "Switch to normal mode.")
   ("open" . "Open a file from disk into the current view.")
   ("open_above" . "Open newline above.")
   ("open_below" . "Open newline below.")
   ("page_cursor_down" . "Page cursor down.")
   ("page_cursor_half_down" . "Page cursor half down.")
   ("page_cursor_half_up" . "Page cursor half up.")
   ("page_cursor_up" . "Page cursor up.")
   ("page_down" . "Page down.")
   ("page_up" . "Page up.")
   ("paste_after" . "Paste after.")
   ("paste_before" . "Paste before.")
   ("paste_clipboard_after" . "Paste system clipboard after.")
   ("paste_clipboard_before" . "Paste system clipboard before.")
   ("paste_primary_clipboard_after" . "Paste system primary clipboard after.")
   ("paste_primary_clipboard_before" . "Paste system primary clipboard before.")
   ("pipe" . "Pipe each selection to the shell command.")
   ("pipe-to" . "Pipe each selection to the shell command, ignoring output.")
   ("primary-clipboard-paste-after" . "Paste primary clipboard after selections.")
   ("primary-clipboard-paste-before" . "Paste primary clipboard before selections.")
   ("primary-clipboard-paste-replace" . "Replace selections with content of system primary clipboard.")
   ("primary-clipboard-yank" . "Yank main selection into system primary clipboard.")
   ("primary-clipboard-yank-join" . "Yank joined selections into system primary clipboard. A separator can be provided as first argument. Default value is newline.")
   ("push-range-to-selection!" . "Push range to selection.")
   ("quit" . "Close the current view.")
   ("quit!" . "Force close the current view, ignoring unsaved changes.")
   ("quit-all" . "Close all views.")
   ("quit-all!" . "Force close all views ignoring unsaved changes.")
   ("range" . "Create range.")
   ("range->from" . "Get range from.")
   ("range->selection" . "Get range selection.")
   ("range->span" . "Get range span.")
   ("range->to" . "Get range to.")
   ("range-anchor" . "Get range anchor.")
   ("range-head" . "Get range head.")
   ("read" . "Load a file into buffer")
   ("record_macro" . "Record_macro.")
   ("redo" . "Redo last change.")
   ("redraw" . "Clear and re-render the whole UI")
   ("reflow" . "Hard-wrap the current selection of lines to a given width.")
   ("regex-selection" . "Select by regex.")
   ("reload" . "Discard changes and reload from the source file.")
   ("reload-all" . "Discard changes and reload all documents from the source files.")
   ("remove-current-selection-range!" . "Remove current selection range.")
   ("remove_primary_selection" . "Remove primary selection.")
   ("remove_selections" . "Remove selections.")
   ("rename_symbol" . "Rename symbol.")
   ("repeat_last_motion" . "Repeat last motion.")
   ("replace" . "Replace selection.")
   ("replace-selection-with" . "Replace selection with string.")
   ("replace_selections_with_clipboard" . "Replace selections with system clipboard.")
   ("replace_selections_with_primary_clipboard" . "Replace selections with system primary clipboard.")
   ("replace_with_yanked" . "Replace selection with yanked.")
   ("replay_macro" . "Replay macro.")
   ("reset-diff-change" . "Reset the diff change at the cursor position.")
   ("reverse_selection_contents" . "Reverse selection contents.")
   ("rotate_selection_contents_backward" . "Rotate selection contents backward.")
   ("rotate_selection_contents_forward" . "Rotate selection contents forward.")
   ("rotate_selections_backward" . "Rotate selections backward.")
   ("rotate_selections_first" . "Rotate selections first.")
   ("rotate_selections_forward" . "Rotate selections forward.")
   ("rotate_selections_last" . "Rotate selections last.")
   ("rotate_view" . "Rotate view.")
   ("rotate_view_reverse" . "Rotate view reverse.")
   ("rsearch" . "Reverse search.")
   ("run-shell-command" . "Run a shell command")
   ("save_selection" . "Save selection.")
   ("scroll_down" . "Scroll down.")
   ("scroll_up" . "Scroll up.")
   ("search" . "Search.")
   ("search_next" . "Search next.")
   ("search_prev" . "Search prev.")
   ("search_selection" . "Search selection.")
   ("search_selection_detect_word_boundaries" . "Search selection with word boundaries.")
   ("select_all" . "Select all.")
   ("select_all_children" . "Select all children.")
   ("select_all_siblings" . "Select all siblings.")
   ("select_line_above" . "Select line above.")
   ("select_line_below" . "Select line below.")
   ("select_mode" . "Switch to select mode.")
   ("select_next_sibling" . "Select next sibling.")
   ("select_prev_sibling" . "Select prev sibling.")
   ("select_references_to_symbol_under_cursor" . "Select references to symbol under cursor.")
   ("select_regex" . "Select by regex.")
   ("select_register" . "Select register.")
   ("select_textobject_around" . "Select textobject around.")
   ("select_textobject_inner" . "Select textobject inner.")
   ("selection->primary-index" . "Get primary index of selection.")
   ("selection->primary-range" . "Get primary range of selection.")
   ("selection->ranges" . "Get ranges of selection.")
   ("set-current-selection-object!" . "Set current selection object.")
   ("set-current-selection-primary-index!" . "Set current selection primary index.")
   ("set-language" . "Set the language of current buffer (show current language if no value specified).")
   ("set-option" . "Set a config option at runtime.\nFor example to disable smart case search, use `:set search.smart-case false`.")
   ("set-register" . "Set contents of the given register.")
   ("shell_append_output" . "Shell append output.")
   ("shell_insert_output" . "Shell insert output.")
   ("shell_keep_pipe" . "Shell keep pipe.")
   ("shell_pipe" . "Shell pipe each selection.")
   ("shell_pipe_to" . "Shell pipe to command.")
   ("show-clipboard-provider" . "Show clipboard provider name in status bar.")
   ("show-directory" . "Show the current working directory.")
   ("shrink_selection" . "Shrink selection.")
   ("shrink_to_line_bounds" . "Shrink to line bounds.")
   ("signature_help" . "Open signature help.")
   ("smart_tab" . "Smart tab.")
   ("sort" . "Sort ranges in selection.")
   ("split_selection" . "Split selection.")
   ("split_selection_on_newline" . "Split selection on newline.")
   ("surround_add" . "Add surrounding.")
   ("surround_delete" . "Delete surrounding.")
   ("surround_replace" . "Replace surrounding.")
   ("suspend" . "Suspend Helix.")
   ("swap_view_down" . "Swap view down.")
   ("swap_view_left" . "Swap view left.")
   ("swap_view_right" . "Swap view right.")
   ("swap_view_up" . "Swap view up.")
   ("switch_case" . "Switch case.")
   ("switch_to_lowercase" . "Switch to lowercase.")
   ("switch_to_uppercase" . "Switch to uppercase.")
   ("symbol_picker" . "Open symbol picker.")
   ("syntax_symbol_picker" . "Open syntax symbol picker.")
   ("syntax_workspace_symbol_picker" . "Open syntax workspace symbol picker.")
   ("theme" . "Change the editor theme (show current theme if no name specified).")
   ("till_prev_char" . "Till prev char.")
   ("toggle-option" . "Toggle a config option at runtime.\nFor example to toggle smart case search, use `:toggle search.smart-case`.")
   ("toggle_block_comments" . "Toggle block comments.")
   ("toggle_comments" . "Toggle comments.")
   ("toggle_line_comments" . "Toggle line comments.")
   ("transpose_view" . "Transpose view.")
   ("tree-sitter-highlight-name" . "Display name of tree-sitter highlight scope under the cursor.")
   ("tree-sitter-layers" . "Display language names of tree-sitter injection layers under the cursor.")
   ("tree-sitter-scopes" . "Display tree sitter scopes, primarily for theming and development.")
   ("tree-sitter-subtree" . "Display the smallest tree-sitter subtree that spans the primary selection, primarily for debugging queries.")
   ("trim_selections" . "Trim selections.")
   ("tutor" . "Open the tutorial.")
   ("undo" . "Undo last change.")
   ("unindent" . "Unindent selection.")
   ("update" . "Write changes only if the file has been modified.")
   ("vsplit" . "Open the file in a vertical split.")
   ("vsplit-new" . "Open a scratch buffer in a vertical split.")
   ("vsplit_new" . "Open a scratch buffer in a vertical split.")
   ("write" . "Write changes to disk. Accepts an optional path (:write some/path.txt)")
   ("write!" . "Force write changes to disk creating necessary subdirectories. Accepts an optional path (:write! some/path.txt)")
   ("write-all" . "Write changes from all buffers to disk.")
   ("write-all!" . "Forcefully write changes from all buffers to disk creating necessary subdirectories.")
   ("write-buffer-close" . "Write changes to disk and closes the buffer. Accepts an optional path (:write-buffer-close some/path.txt)")
   ("write-buffer-close!" . "Force write changes to disk creating necessary subdirectories and closes the buffer. Accepts an optional path (:write-buffer-close! some/path.txt)")
   ("write-quit" . "Write changes to disk and close the current view. Accepts an optional path (:wq some/path.txt)")
   ("write-quit!" . "Write changes to disk and close the current view forcefully. Accepts an optional path (:wq! some/path.txt)")
   ("write-quit-all" . "Write changes from all buffers to disk and close all views.")
   ("write-quit-all!" . "Forcefully write changes from all buffers to disk, creating necessary subdirectories, and close all views (ignoring unsaved changes).")
   ("x" . "Alias for :exit. Write changes to disk if the buffer is modified and then quit. Accepts an optional path (:exit some/path.txt).")
   ("x!" . "Alias for :exit!. Force write changes to disk, creating necessary subdirectories, if the buffer is modified and then quit. Accepts an optional path (:exit! some/path.txt).")
   ("xa" . "Alias for :write-quit-all. Write changes from all buffers to disk and close all views.")
   ("xa!" . "Alias for :write-quit-all!. Forcefully write changes from all buffers to disk, creating necessary subdirectories, and close all views (ignoring unsaved changes).")
   ("xit" . "Alias for :exit. Write changes to disk if the buffer is modified and then quit. Accepts an optional path (:exit some/path.txt).")
   ("xit!" . "Alias for :exit!. Force write changes to disk, creating necessary subdirectories, if the buffer is modified and then quit. Accepts an optional path (:exit! some/path.txt).")
   ("yank" . "Yank selection.")
   ("yank-diagnostic" . "Yank diagnostic(s) under primary cursor to register, or clipboard by default")
   ("yank-join" . "Yank joined selections. A separator can be provided as first argument. Default value is newline.")
   ("yank_joined" . "Yank joined.")
   ("yank_joined_to_clipboard" . "Yank joined to clipboard.")
   ("yank_joined_to_primary_clipboard" . "Yank joined to primary clipboard.")
   ("yank_main_selection_to_clipboard" . "Yank main selection to clipboard.")
   ("yank_main_selection_to_primary_clipboard" . "Yank main selection to primary clipboard.")
   ("yank_to_clipboard" . "Yank to clipboard.")
   ("yank_to_primary_clipboard" . "Yank to primary clipboard.")
   ("workspace_diagnostics_picker" . "Open workspace diagnostics picker.")
   ("workspace_symbol_picker" . "Open workspace symbol picker.")))

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

; Calculate a score for fuzzy matching: higher is better
; 1000 = exact match
; 80 = prefix match
; 50 = word boundary match (start of word after - or _)
; 10 = standard subsequence match
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
         [(= ti text-len) 0] ; Failed to match all characters
         [else
          (let ([p-char (char-downcase (string-ref pattern pi))]
                [t-char (char-downcase (string-ref text ti))])
            (if (char=? p-char t-char)
                (let ([new-score (+ score (if at-boundary 50 10))])
                  (loop (+ pi 1) (+ ti 1) new-score #f))
                (let ([is-boundary (or (char=? t-char #\-) (char=? t-char #\_) (char=? t-char #\/))])
                  (loop pi (+ ti 1) score is-boundary))))]))]))

(define (fuzzy-match? pattern text)
  (> (fuzzy-score pattern text) 0))

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

(define (string-ends-with? s suffix)
  (let ([slen (string-length s)]
        [sublen (string-length suffix)])
    (if (< slen sublen)
        #f
        (string=? (substring s (- slen sublen) slen) suffix))))

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

(define (get-setting-completions arg-to-complete prefix)
  (let ([settings '("indent-heuristic" "atomic-save" "lsp" "search" "auto-pairs" "continue-comments" "popup-border" "cursor-shape" "whitespace" "indent-guides" "scrolloff" "scroll_lines" "mouse" "shell" "jump-label-alphabet" "line-number" "cursorline" "cursorcolumn" "middle-click-paste" "auto-completion" "auto-format" "auto-save" "text-width" "idle-timeout" "completion-timeout" "preview-completion-insert" "completion-trigger-len" "completion-replace" "auto-info" "true-color" "insert-final-newline" "color-modes" "gutters" "undercurl" "terminal" "rulers" "bufferline" "workspace-lsp-roots" "default-line-ending" "smart-tab" "rainbow-brackets")])
    (map (lambda (s) (string-append prefix s))
         (filter (lambda (s) (fuzzy-match? arg-to-complete s)) settings))))

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
         (filter (lambda (e) (fuzzy-match? file-p (file-name e))) entries))))

(define (get-argument-completions cmd last-part prefix parts)
  (let* ([resolved (resolve-shorthand cmd)]
         [is-at-end (string=? last-part "")]
         [num-full-args (if is-at-end (- (length parts) 1) (- (length parts) 2))])
    (cond
      [(member resolved '("theme"))
       (if (> num-full-args 0)
           '()
           (let ([themes (themes->list)])
             (map (lambda (t) (string-append prefix t))
                  (sort (filter (lambda (t) (fuzzy-match? last-part t)) themes)
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

(define (get-completions input all-commands-with-docs)
  (define all-commands (map car all-commands-with-docs))
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
             
             (get-argument-completions cmd last-part prefix parts))]
          
          ; Command completion
          [(string=? base-word "") '()]
          [else 
           (let* ([shorthand-target (resolve-shorthand base-word)]
                  [has-exact-shorthand? (and (not (equal? shorthand-target base-word)) 
                                             (member shorthand-target all-commands))]
                  [scored-matches (map (lambda (cmd) (cons cmd (fuzzy-score base-word cmd))) all-commands)]
                  [filtered-matches (filter (lambda (p) (> (cdr p) 0)) scored-matches)]
                  [sorted-matches (sort filtered-matches (lambda (a b) (> (cdr a) (cdr b))))]
                  [matches (map car sorted-matches)]
                  ; Prioritize exact shorthand target if it exists
                  [final-matches (if has-exact-shorthand?
                                    (cons shorthand-target (filter (lambda (m) (not (string=? m shorthand-target))) matches))
                                    matches)])
             (if has-bang?
                 (map (lambda (m) (string-append m "!")) final-matches)
                 final-matches))]))))

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

(define (render-highlighted-string frame x y text indices base-style highlight-style width)
  (define len (string-length text))
  (let loop ([i 0] [curr-x x] [rem-indices indices])
    (cond
      [(= i len) 
       ; Fill the rest of the line with base-style
       (when (< (- curr-x x) width)
         (frame-set-string! frame curr-x y (apply string-append (map (lambda (_) " ") (range 0 (- width (- curr-x x))))) base-style))]
      [(and (not (null? rem-indices)) (= i (car rem-indices)))
       ; Find end of consecutive highlighted indices
       (let h-loop ([j i] [rj rem-indices])
         (if (and (< j len) (not (null? rj)) (= j (car rj)))
             (h-loop (+ j 1) (cdr rj))
             (begin
               (frame-set-string! frame curr-x y (substring text i j) highlight-style)
               (loop j (+ curr-x (- j i)) rj))))]
      [else
       ; Find end of consecutive non-highlighted characters
       (let n-loop ([j i])
         (if (or (= j len) (and (not (null? rem-indices)) (= j (car rem-indices))))
             (begin
               (frame-set-string! frame curr-x y (substring text i j) base-style)
               (loop j (+ curr-x (- j i)) rem-indices))
             (n-loop (+ j 1))))])))

(define (cmdline-render state rect frame)
  (define width (area-width rect))
  (define height (area-height rect))
  
  (define input-field (CmdlineState-input state))
  (define input-str (text-field->string input-field))
  (define completions (unbox (CmdlineState-completions state)))
  (define completion-index (unbox (CmdlineState-completion-index state)))
  (define all-commands (CmdlineState-all-commands state))
  
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
  (define auto-execute (hash-get *fine-cmdline-config* "auto-execute-first"))
  
  ; Use native helix theme styles
  (define bg-theme-style (theme-scope bg-scope))
  (define bg-color (style->bg bg-theme-style))
  
  ; Create styles that use the configured background
  (define prompt-style (~> (theme-scope "keyword") (style-bg bg-color)))
  (define input-style (~> (theme-scope "ui.text") (style-bg bg-color)))
  (define base-bg-style (~> (theme-scope "ui.background") (style-bg bg-color)))
  
  ; Completions use the configured background
  (define completion-style (~> (theme-scope "ui.menu") (style-bg bg-color)))
  
  ; Fuzzy Highlight Style (foreground from 'special')
  (define fuzzy-highlight-fg (style->fg (theme-scope "special")))
  
  ; Selection/Active Styles
  ; If manually selected (Tab): Use ui.menu.selected
  (define manual-selected-style (theme-scope "ui.menu.selected"))
  
  ; If auto-active (first match): Use special color as background
  ; We use ui.background color for text on top of the special background
  (define auto-active-style (~> completion-style (style-bg fuzzy-highlight-fg) (style-fg bg-color)))
  
  (define directory-style (~> (theme-scope "ui.text.directory") (style-bg bg-color)))
  
  ; Border style
  (define border-theme-style (theme-scope border-scope))
  (define border-style (~> border-theme-style (style-bg bg-color)))
  
  ; Dynamic height based on completions
  (define num-completions (length completions))
  (define has-completions? (and (> num-completions 0) (> (string-length input-str) 0)))
  
  ; Determine if we are in argument mode
  (define parts (string-split-manual input-str))
  (define is-arg-mode (or (> (length parts) 1) 
                          (and (> (string-length input-str) 0) 
                               (char=? (string-ref input-str (- (string-length input-str) 1)) #\space))))
  
  ; completion-height is number of completion rows to display
  (define completion-height (if has-completions?
                               (min num-completions max-completions)
                               0))
  
  ; Documentation tooltip
  (define selected-doc 
    (if (and has-completions? (>= completion-index 0) (< completion-index num-completions))
        (let* ([comp-name (list-ref completions completion-index)]
               [doc-pair (assoc comp-name all-commands)])
          (if doc-pair (cdr doc-pair) #f))
        (if (and has-completions? (= completion-index -1) auto-execute (not is-arg-mode))
            (let* ([comp-name (car completions)]
                   [doc-pair (assoc comp-name all-commands)])
              (if doc-pair (cdr doc-pair) #f))
            #f)))
  
  (define show-doc? (not (is-false? selected-doc)))
  (define doc-height (if show-doc? 2 0)) ; 1 line doc + 1 line separator/padding
  
  ; Border adjustments
  (define padding (if show-border 1 0))
  (define cmd-height (+ (* padding 2) 1 completion-height doc-height))
  
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
    
    ; Determine pattern for highlighting
    (let* ([last-part (if (char=? (string-ref input-str (- (string-length input-str) 1)) #\space)
                          ""
                          (if (null? parts) "" (list-ref parts (- (length parts) 1))))]
           [pattern (if is-arg-mode last-part (if (null? parts) "" (car parts)))])
      
      (for-index (lambda (i comp)
                   (define actual-index (+ i start))
                   (define is-tab-selected (= actual-index completion-index))
                   (define is-auto-active (and (= completion-index -1) (= actual-index 0) auto-execute (not is-arg-mode)))
                   
                   ; Check if it's a directory (ends with /) for syntax highlighting
                   (define is-dir (and (> (string-length comp) 0) 
                                       (char=? (string-ref comp (- (string-length comp) 1)) #\/)))
                   
                   ; Get indices for highlighting
                   (define indices (get-fuzzy-indices pattern comp))

                   ; Truncate if longer than width - avoid overflowing borders
                   (define max-comp-len (- cmd-width (* padding 2) 1))
                   (define display-comp (if (> (string-length comp) max-comp-len)
                                           (string-append (substring comp 0 (- max-comp-len 3)) "...")
                                           comp))
                   
                   (define base-comp-style (cond [is-tab-selected manual-selected-style]
                                                [is-auto-active auto-active-style]
                                                [is-dir directory-style]
                                                [else completion-style]))
                   
                   ; Matched chars highlighting:
                   ; If row is highlighted, matched chars should probably just be regular text (since bg is highlight)
                   ; but to keep visual consistency, we can use a slightly different style or keep it same as base
                   (define match-char-style (cond 
                                              [is-tab-selected manual-selected-style]
                                              [is-auto-active auto-active-style]
                                              [else (~> base-comp-style (style-fg fuzzy-highlight-fg))]))

                   (frame-set-string! frame (+ (area-x cmd-area) padding) (+ completion-y i) " " base-comp-style)
                   (render-highlighted-string frame 
                                             (+ (area-x cmd-area) padding 1) 
                                             (+ completion-y i) 
                                             display-comp 
                                             indices 
                                             base-comp-style 
                                             match-char-style
                                             (- cmd-width (* padding 2) 1)))
                 completion-list
                 0))
    
    ; Render documentation tooltip
    (when show-doc?
      (define doc-y (+ completion-y completion-height))
      ; Render a small separator
      (frame-set-string! frame
                        (+ (area-x cmd-area) padding)
                        doc-y
                        (apply string-append (map (lambda (x) "─") (range 0 (- cmd-width (* padding 2)))))
                        border-style)
      ; Render doc text
      (define max-doc-len (- cmd-width (* padding 2) 2))
      (define display-doc (if (> (string-length selected-doc) max-doc-len)
                             (string-append (substring selected-doc 0 (- max-doc-len 3)) "...")
                             selected-doc))
      (frame-set-string! frame
                        (+ (area-x cmd-area) padding 1)
                        (+ doc-y 1)
                        display-doc
                        (~> (theme-scope "ui.text.info") (style-bg bg-color))))))

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
       (let* ([new-history (cons input-str history)])
         (set-CmdlineState-history! state new-history)
         (save-history-to-disk new-history))
       
       (define completions (unbox completions-box))
       (define completion-index (unbox completion-index-box))
       
       ; Check if we are in argument mode
       (define parts (string-split-manual input-str))
       (define is-arg-mode (or (> (length parts) 1) 
                               (char=? (string-ref input-str (- (string-length input-str) 1)) #\space)))

       ; Execution logic:
       ; 1. If an item is explicitly selected (index != -1), use it.
       ; 2. If no item selected, and it's a command (not arg mode), and auto-execute is true, use first match.
       ; 3. Otherwise, use literal input.
       (define cmd-to-execute
         (cond
           [(and (not (null? completions)) (not (= completion-index -1)))
            (list-ref completions completion-index)]
           [(and (not (null? completions)) (not is-arg-mode) auto-execute)
            (car completions)]
           [else input-str]))
             
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
           (cond
             [(> h-index 1)
              (set-box! history-index (- h-index 1))
              (define hist-cmd (list-ref history (- h-index 2)))
              (set-MutableTextField-text! input-field (reverse (string->list hist-cmd)))]
             [(= h-index 1)
              (set-box! history-index 0)
              (set-MutableTextField-text! input-field '())]
             [else void])))
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
  (define initial-completions (box (map car all-cmds)))
  (define history (load-history-from-disk))
  
  (push-component! 
   (new-component! "fine-cmdline"
                   (CmdlineState input-field
                                history
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
