{ pkgs, emacsPkg, ... }:
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  # Set TMPDIR for launchd agent so emacs daemon creates socket in /tmp
  launchd.agents.emacs.config.EnvironmentVariables = {
    TMPDIR = "/tmp";
  };
}
