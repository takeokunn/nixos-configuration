{ pkgs, nodePkgs }:
let
  platformPackages = {
    "x86_64-linux" = "@github/copilot-language-server-linux-x64";
    "aarch64-linux" = "@github/copilot-language-server-linux-arm64";
    "x86_64-darwin" = "@github/copilot-language-server-darwin-x64";
    "aarch64-darwin" = "@github/copilot-language-server-darwin-arm64";
  };
  platformPkg = nodePkgs.${platformPackages.${pkgs.stdenv.hostPlatform.system}};
in
{
  home.packages = [
    nodePkgs."@github/copilot-language-server"
    platformPkg
  ];

  programs.fish = {
    interactiveShellInit = ''
      set -gx COPILOT_LANGUAGE_SERVER_PATH ${platformPkg}/lib/node_modules/${
        platformPackages.${pkgs.stdenv.hostPlatform.system}
      }/copilot-language-server
    '';
  };
}
