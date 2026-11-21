{ pkgs, ... }:
let
  inherit (pkgs) lib;
  isLinux = pkgs.stdenv.hostPlatform.isLinux;
  # Electron picks its password store from the desktop environment and does not recognize niri,
  # so it would fall back to the unencrypted basic store.
  passwordStore = "--password-store=gnome-libsecret";
  # slack's package takes no commandLineArgs, so the flag is added by a wrapper.
  slack = pkgs.symlinkJoin {
    name = "slack-${pkgs.slack.version}";
    paths = [ pkgs.slack ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/slack --add-flags ${passwordStore}
    '';
  };
in
{
  programs.discord = {
    enable = true;
    package = lib.mkIf isLinux (pkgs.discord.override { commandLineArgs = passwordStore; });
    settings = {
      SKIP_HOST_UPDATE = true;
      OPEN_ON_STARTUP = false;
    };
  };

  home.packages = lib.optionals isLinux [ slack ];
}
