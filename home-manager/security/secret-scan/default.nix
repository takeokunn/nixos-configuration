{
  config,
  lib,
  pkgs,
  ...
}:
let
  stateDir = "${config.home.homeDirectory}/.local/state/secret-scan";
  secretScan = import ./secret-scan.nix { inherit pkgs; };
in
lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
  # launchd does not create the directory of StandardOutPath.
  home.activation.secretScanStateDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run mkdir -p -m 700 ${lib.escapeShellArg stateDir}
  '';

  launchd.agents.secret-scan = {
    enable = true;
    config = {
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path /nix/store && exec ${secretScan}"
      ];
      # A run missed while the machine sleeps starts on wake.
      StartCalendarInterval = [
        {
          Hour = 12;
          Minute = 30;
        }
      ];
      StandardOutPath = "${stateDir}/stdout.log";
      StandardErrorPath = "${stateDir}/stderr.log";
      ProcessType = "Background";
      LowPriorityIO = true;
      Nice = 10;
    };
  };
}
