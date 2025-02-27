{ pkgs, nodePkgs }:
let
  platforms = {
    "x86_64-linux" = "linux-amd64";
    "aarch64-linux" = "linux-aarch64";
    "x86_64-darwin" = "darwin-amd64";
    "aarch64-darwin" = "darwin-arm64";
  };
  platform = builtins.getAttr pkgs.system platforms;
in
{
  home.packages = [ nodePkgs."@github/copilot-language-server" ];

  programs.fish = {
    interactiveShellInit = ''
      set -gx COPILOT_LANGUAGE_SERVER_PATH ${
        nodePkgs."@github/copilot-language-server"
      }/lib/node_modules/@github/copilot-language-server/native/${platform}/copilot-language-server
    '';
  };
}
