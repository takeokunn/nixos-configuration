{ pkgs, ... }:
let
  lib = pkgs.lib;
  isDarwin = pkgs.stdenv.isDarwin;
in
{
  home.sessionVariables.SSH_AUTH_SOCK = "$HOME/.gnupg/S.gpg-agent.ssh";

  services.gpg-agent.enable = true;
  services.gpg-agent.pinentry.package = pkgs.pinentry-curses;
  services.gpg-agent.defaultCacheTtl = 60 * 60 * 24;
  services.gpg-agent.defaultCacheTtlSsh = 60 * 60 * 24;
  services.gpg-agent.maxCacheTtl = 60 * 60 * 24;
  services.gpg-agent.maxCacheTtlSsh = 60 * 60 * 24;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.enableExtraSocket = true;

  launchd.agents.gpg-agent.config.ProgramArguments = lib.mkIf isDarwin (
    lib.mkForce [
      "/bin/sh"
      "-c"
      "/bin/wait4path /nix/store && exec ${pkgs.gnupg}/bin/gpg-agent --daemon"
    ]
  );

  home.activation.restartGpgAgent = lib.mkIf isDarwin {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      ${pkgs.gnupg}/bin/gpgconf --kill gpg-agent 2>/dev/null || true
      ${pkgs.gnupg}/bin/gpg-agent --daemon 2>/dev/null || true
    '';
  };

  programs.fish.interactiveShellInit = ''
    set -x SSH_AUTH_SOCK $(gpgconf --list-dirs agent-ssh-socket)
  '';
}
