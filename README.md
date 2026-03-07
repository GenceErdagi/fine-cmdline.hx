# helix-fine-cmdline

A fine-grained command line input for Helix, inspired by [fine-cmdline.nvim](https://github.com/vonheikemen/fine-cmdline.nvim).

## Features

- Floating window command input
- Fuzzy command completion
- Command history navigation (up/down arrows)
- Tab completion cycling
- Customizable keybindings

## Installation

### Using helix-config

If you use the helix-config system, add this to your config:

```scheme
; In your init.scm or helix.scm
(require "path/to/fine-cmdline")

; Load the package
(load-package "fine-cmdline.scm")
```

### Manual Setup

1. Copy `fine-cmdline.scm` to your helix configuration directory (e.g., `~/.config/helix/` or where your Steel scripts are loaded from)
2. Add the keybinding

## Usage

### Opening the Command Line

Bind a key to open the fine command line. Add to your Steel config:

```scheme
; In init.scm or similar
(require "fine-cmdline")

; Example: Bind to <Space>:
(define-key "normal" "space" ":fine-cmdline/open")
```

### Keybindings

| Key | Action |
|-----|--------|
| `Enter` | Execute command |
| `Escape` | Close command line |
| `Backspace` | Delete character |
| `Tab` | Cycle completions forward |
| `Shift+Tab` | Cycle completions backward |
| `Up` | Previous command in history |
| `Down` | Next command in history |
| `a-z, A-Z, 0-9` | Type command |

## Configuration

### Custom Commands List

You can modify the `get-all-commands` function in `fine-cmdline.scm` to add or remove commands from the completion list.

### Command Execution

Currently commands are executed via helix's built-in command runner. The command string is passed to helix with a `:` prefix.

## Requirements

- Helix with Steel plugin system enabled
- Steel runtime

## See Also

- Original Neovim plugin: [fine-cmdline.nvim](https://github.com/vonheikemen/fine-cmdline.nvim)
- Helix: https://helix-editor.com/
- Steel: https://github.com/helix-editor/helix/wiki/Steel
