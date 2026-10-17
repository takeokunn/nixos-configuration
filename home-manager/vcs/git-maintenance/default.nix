{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.git-maintenance;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  # Neither launchd agents nor systemd user units see shell session
  # variables, and the credential helper scripts under ~/.config/git call
  # `gh` by name, so each platform's unit gets an explicit PATH.
  ghBin = lib.makeBinPath [ pkgs.gh ];
in
{
  services.git-maintenance = {
    enable = false;
    ghq.enable = true;
    interval = 604800; # 1 week
    # Private HTTPS remotes can only fetch through the gh credential
    # helpers; the daemon resets every helper unless told otherwise.
    credentialHelpers = true;
    aggressive = true;
    reflogExpire = "now";
    pruneTags = true;
    pruneBranches = true;
    pruneWorktrees = true;
  };

  launchd.agents.git-maintenance.config.EnvironmentVariables = lib.mkIf (cfg.enable && isDarwin) {
    # SSH-remote repos fail fetch without the agent socket set explicitly.
    SSH_AUTH_SOCK = "${config.home.homeDirectory}/.gnupg/S.gpg-agent.ssh";
    PATH = "${ghBin}:/usr/bin:/bin:/usr/sbin:/sbin";
  };

  systemd.user.services.git-maintenance = lib.mkIf (cfg.enable && !isDarwin) {
    # Home Manager's ssh-auth-sock module (enabled by gpg-agent's SSH support)
    # publishes SSH_AUTH_SOCK to the user manager from this oneshot; a daemon
    # started earlier fetches agentless.
    Unit.After = [ "set-SSH_AUTH_SOCK.service" ];
    Service.Environment = [
      "PATH=${ghBin}:/run/wrappers/bin:/etc/profiles/per-user/${config.home.username}/bin:/run/current-system/sw/bin"
    ];
  };
}
