(provide CmdlineState 
         CmdlineState?
         CmdlineState-input
         CmdlineState-history
         CmdlineState-history-index
         CmdlineState-cursor-position
         CmdlineState-completion-index
         CmdlineState-window-start
         CmdlineState-completions
         CmdlineState-all-commands
         CmdlineState-shorthands
         set-CmdlineState-input!
         set-CmdlineState-history!
         set-CmdlineState-history-index!
         set-CmdlineState-cursor-position!
         set-CmdlineState-completion-index!
         set-CmdlineState-window-start!
         set-CmdlineState-completions!
         set-CmdlineState-all-commands!
         set-CmdlineState-shorthands!
         MutableTextField
         MutableTextField?
         MutableTextField-text
         set-MutableTextField-text!)

(struct CmdlineState 
  (input                           ; MutableTextField - user input
   history                         ; List of past commands
   history-index                   ; Box int - current position in history
   cursor-position                 ; Position - where cursor is rendered
   completion-index                ; Box int - selected completion
   window-start                    ; Box int - for scrollable list
   completions                     ; Box list - available completions
   all-commands                    ; Full list of pairs
   shorthands) #:mutable)          ; Hash of shorthands

(struct MutableTextField (text) #:mutable)
