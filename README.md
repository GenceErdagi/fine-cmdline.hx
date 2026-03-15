# fine-cmdline.hx

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

### Position and Size

You can customize the popup position and behavior:

```scheme
(require "fine-cmdline")

; Configure width (default: 60)
(fine-cmdline-config! "width" 80)

; Configure max completions to show (default: 8)
(fine-cmdline-config! "max-completions" 10)

; Position offset from center (use #f for center, or specify offset)
(fine-cmdline-config! "offset-x" -10)  ; 10 chars left of center
(fine-cmdline-config! "offset-y" 5)    ; 5 rows down from center
```

### Custom Commands List

You can modify the `get-all-commands` function in `fine-cmdline.scm` to add or remove commands from the completion list.

### Command Execution

Currently commands are executed via helix's built-in command runner. The command string is passed to helix with a `:` prefix.

## Maintaining Commands

The command list in `fine-cmdline.scm` is derived directly from the Helix source code. To keep the plugin up-to-date with new Helix releases, you can extract the structured command data yourself.

### Source of Truth
All typable commands are defined in the Helix repository at:
`helix-term/src/commands/typed.rs`

The commands are stored in the `TYPABLE_COMMAND_LIST` array. Each entry contains:
- **name**: The canonical command name.
- **aliases**: Shorthands (e.g., `wq`, `vs`).
- **doc**: The help text displayed in the plugin tooltip.
- **completer**: Defines the argument type (filename, buffer, theme, etc.).

### Automated Extraction Script
You can use the following Python script to parse the Helix source and generate the Scheme-compatible lists for `*helix-shorthands*` and `get-all-commands`.

```python
import re
import sys

# Path to your local helix repository
HELIX_SOURCE = "path/to/helix/helix-term/src/commands/typed.rs"

def extract_commands(file_path):
    with open(file_path, "r") as f:
        content = f.read()

    # Matches TypableCommand blocks
    matches = re.finditer(r"TypableCommand\s*\{(.*?)\}", content, re.DOTALL)

    shorthands = []
    commands_with_docs = []

    for match in matches:
        block = match.group(1)
        
        name_match = re.search(r"name:\s*\"([^\"]+)\"", block)
        doc_match = re.search(r"doc:\s*\"([^\"]+)\"", block)
        
        if name_match and doc_match:
            name = name_match.group(1)
            doc = doc_match.group(1).replace("\"", "\\\"")
            
            commands_with_docs.append((name, doc))
            
            aliases_match = re.search(r"aliases:\s*&\[([^\]]+)\]", block)
            if aliases_match:
                aliases_str = aliases_match.group(1)
                aliases = [a.strip().strip("\"") for a in aliases_str.split(",")]
                for alias in aliases:
                    if alias:
                        shorthands.append((alias, name))
                        commands_with_docs.append((alias, f"Alias for :{name}. {doc}"))

    shorthands.sort()
    commands_with_docs.sort()

    print("--- Copy to *helix-shorthands* ---")
    for alias, target in shorthands:
        print(f'        "{alias}" "{target}"')

    print("\n--- Copy to get-all-commands ---")
    for name, doc in commands_with_docs:
        print(f'   ("{name}" . "{doc}")')

if __name__ == "__main__":
    extract_commands(HELIX_SOURCE)
```

## Requirements

- Helix with Steel plugin system enabled
- Steel runtime

## See Also

- Original Neovim plugin: [fine-cmdline.nvim](https://github.com/vonheikemen/fine-cmdline.nvim)
- Helix: https://helix-editor.com/
- Steel: https://github.com/helix-editor/helix/wiki/Steel
