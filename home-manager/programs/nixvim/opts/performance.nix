# ════════════════════════════════════════════════════════════════════════════════
# Performance & UX Options
# ════════════════════════════════════════════════════════════════════════════════
# Timing, scrolling, splits, and general user experience settings.
# Undo persistence is in backup.nix.
# ════════════════════════════════════════════════════════════════════════════════

{
  # Timing: faster CursorHold events (250ms) and key sequence timeout (300ms)
  updatetime = 250;
  timeoutlen = 300;

  # Scrolling: keep 8 lines visible above/below and left/right of cursor
  scrolloff = 8;
  sidescrolloff = 8;

  # Virtual edit: allow cursor beyond line end in visual block mode
  virtualedit = "block";

  # Confirmation dialogs instead of errors for unsaved changes
  confirm = true;

  # Enable mouse support in all modes
  mouse = "a";

  # Completion menu behavior for LSP/mini.completion
  completeopt = "menu,menuone,noselect";

  # Split windows: new splits go below and to the right
  splitbelow = true;
  splitright = true;

  # Show live preview of :substitute in a split window
  inccommand = "split";
}
