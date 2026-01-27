{ pkgs, emacsPkg, ... }:
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  # macOS: Set TMPDIR for launchd agent so emacs daemon creates socket in /tmp
  # (macOS doesn't have XDG_RUNTIME_DIR, so Emacs uses TMPDIR)
  launchd.agents.emacs.config.EnvironmentVariables = {
    TMPDIR = "/tmp";
  };

  # Linux: Emacs 28+ uses XDG_RUNTIME_DIR/emacs by default, no override needed
}
