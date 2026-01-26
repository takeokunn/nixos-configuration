# Emacs constants - can be imported without pkgs/emacsPkg dependencies
# These are the single source of truth for Emacs configuration values
{
  # Socket path for Emacs daemon
  # macOS launchd agent sets TMPDIR=/tmp, so socket is created here
  # Format uses shell variable expansion: $(id -u) gets current user ID
  socketPath = "/tmp/emacs$(id -u)/server";

  # Default window dimensions for scratchpad
  defaultWindowWidth = 900;
  defaultWindowHeight = 600;

  # Default app identifier for floating Emacs scratchpad
  defaultAppId = "FloatingEmacs";
}
