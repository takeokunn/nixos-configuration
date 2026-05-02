# ════════════════════════════════════════════════════════════════════════════════
# NixVim Options Aggregator
# ════════════════════════════════════════════════════════════════════════════════
# Merges all option categories into a single attribute set.
# Categories:
#   - display.nix:     Line numbers, statusline, colors, cursor, signs
#   - editing.nix:     Encoding, indentation, backspace
#   - search.nix:      Search behavior, folding
#   - performance.nix: Timing, scrolling, splits, UX
#   - clipboard.nix:   Clipboard integration
#   - backup.nix:      Swap files, undo persistence
#   - filetype.nix:    Filetype detection (currently empty, for future use)
# ════════════════════════════════════════════════════════════════════════════════

let
  display = import ./display.nix;
  editing = import ./editing.nix;
  search = import ./search.nix;
  performance = import ./performance.nix;
  clipboard = import ./clipboard.nix;
  backup = import ./backup.nix;
  filetype = import ./filetype.nix;
in
display // editing // search // performance // clipboard // backup // filetype
