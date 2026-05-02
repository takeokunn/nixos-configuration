# ════════════════════════════════════════════════════════════════════════════════
# Backup & Undo Options
# ════════════════════════════════════════════════════════════════════════════════
# Options for backup files, swap files, and persistent undo.
# ════════════════════════════════════════════════════════════════════════════════

{
  # Disable swap files (rely on git and undo history instead)
  swapfile = false;

  # Enable persistent undo (survives vim restarts)
  undofile = true;
  undolevels = 10000;
}
