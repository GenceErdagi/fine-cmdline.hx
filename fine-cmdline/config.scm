(provide *fine-cmdline-config*
         fine-cmdline-config!
         fine-cmdline-config-getter)

(define *fine-cmdline-config* 
  (hash "width" 100
        "max-completions" 10
        "offset-x" #f
        "offset-y" #f
        "anchor" 'top
        "show-title" #t
        "title-text" " Cmdline "
        "prompt-text" ": "
        "show-border" #t
        "border-type" "plain"
        "bg-scope" "ui.menu"
        "border-scope" "ui.background"
        "fill-on-tab" #t
        "auto-execute-first" #t))

(define (fine-cmdline-config! key value)
  (set! *fine-cmdline-config* (hash-insert *fine-cmdline-config* key value)))

(define (fine-cmdline-config-getter key)
  (hash-get *fine-cmdline-config* key))
