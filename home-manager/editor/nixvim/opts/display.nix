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

  showmatch = true;
  showcmd = true;
  title = true;

  termguicolors = true;

  cursorline = true;

  # Sign column: always show with space for 2 signs (git + diagnostics)
  signcolumn = "yes:2";
}
