# ════════════════════════════════════════════════════════════════════════════════
# Display Options
# ════════════════════════════════════════════════════════════════════════════════
# Visual display settings: line numbers, statusline, colors, cursor, and signs.
# ════════════════════════════════════════════════════════════════════════════════

{
  # Explicitly set dark background to prevent OSC 11 terminal queries
  background = "dark";

  # Line numbers: disabled (using relative context from mini.nvim instead)
  number = false;
  relativenumber = false;

  # Global statusline (single statusline across all splits)
  laststatus = 3;
  # Minimal statusline content (mini.statusline handles the rest)
  statusline = "%y";

  # Visual aids: show matching brackets, display partial commands, set window title
  showmatch = true;
  showcmd = true;
  title = true;

  # True color support (required for modern colorschemes)
  termguicolors = true;

  # Highlight current line
  cursorline = true;

  # Sign column: always show with space for 2 signs (git + diagnostics)
  signcolumn = "yes:2";
}
