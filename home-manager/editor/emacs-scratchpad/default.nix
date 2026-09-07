{ pkgs, emacsLib, ... }:
let
  lib = pkgs.lib;
in
{
  launchd.agents.emacs-scratchpad-kitty = lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    domain = "gui";
    config = {
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path /nix/store && exec ${emacsLib.scratchpadKittyServer}"
      ];
      # Not KeepAlive: when a hotkey-spawned kitty already owns the instance
      # group, this process acts as a client and exits, which KeepAlive would
      # turn into a relaunch loop.
      RunAtLoad = true;
      KeepAlive = false;
      ProcessType = "Interactive";
    };
  };
}
