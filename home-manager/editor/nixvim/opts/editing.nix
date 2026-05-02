# ════════════════════════════════════════════════════════════════════════════════
# Editing Options
# ════════════════════════════════════════════════════════════════════════════════
# Core text editing behavior: encoding, indentation, and backspace handling.
# Clipboard and backup settings are in their respective files.
# ════════════════════════════════════════════════════════════════════════════════

{
  # Character encoding (UTF-8 primary, with Japanese fallbacks)
  encoding = "utf-8";
  fileencodings = "utf-8,euc-jp,cp932";

  # Backspace behavior: allow backspace over indent, line breaks, and insert start
  backspace = "indent,eol,start";

  # Indentation: 2 spaces, expand tabs to spaces
  tabstop = 2;
  shiftwidth = 2;
  expandtab = true;
}
