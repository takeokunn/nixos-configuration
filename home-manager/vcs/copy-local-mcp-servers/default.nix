{ config, pkgs, ... }:
{
  home.packages = [
    (pkgs.writeShellApplication {
      name = "copy-local-mcp-servers";
      # claude is not a runtime input: the script uses the claude on the caller's PATH.
      runtimeInputs = [
        pkgs.jq
        config.programs.jujutsu.package
        pkgs.coreutils
      ];
      text = builtins.readFile ./copy-local-mcp-servers.sh;
    })
  ];
}
