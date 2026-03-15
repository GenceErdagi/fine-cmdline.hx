(require "helix/components.scm")
(require "helix/misc.scm")
(require "fine-cmdline/utils.scm")
(require "fine-cmdline/types.scm")
(require "fine-cmdline/config.scm")
(require "fine-cmdline/logic.scm")
(require "fine-cmdline/data.scm")

(provide fine-cmdline
         fine-cmdline/open
         fine-cmdline-config!
         fine-cmdline-config-getter
         ; Re-export logic types for external use
         CmdlineState 
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

(define (fine-cmdline/open)
  (define commands-cache (get-all-commands-data))
  (define shorthands-cache *helix-shorthands*)
  
  (define input-field (MutableTextField '()))
  (define history (load-history-from-disk))
  (define initial-completions (box (map car commands-cache)))
  
  (push-component! 
   (new-component! "fine-cmdline"
                   (CmdlineState input-field
                                history
                                (box 0)
                                (position 0 0)
                                (box -1)
                                (box 0)
                                initial-completions
                                commands-cache
                                shorthands-cache)
                   cmdline-render
                   (hash "handle_event" cmdline-event-handler
                         "cursor" cmdline-cursor-handler))))

(define fine-cmdline fine-cmdline/open)
