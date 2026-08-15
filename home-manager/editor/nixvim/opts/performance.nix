{
  # Timing: faster CursorHold events (250ms) and key sequence timeout (300ms)
  updatetime = 250;
  timeoutlen = 300;

  scrolloff = 8;
  sidescrolloff = 8;

  # Virtual edit: allow cursor beyond line end in visual block mode
  virtualedit = "block";

  # Confirmation dialogs instead of errors for unsaved changes
  confirm = true;

  # Enable mouse support in all modes
  mouse = "a";

  completeopt = "menu,menuone,noselect";

  splitbelow = true;
  splitright = true;

  # Show live preview of :substitute in a split window
  inccommand = "split";
}
