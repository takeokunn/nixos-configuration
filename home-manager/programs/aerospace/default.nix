{ pkgs }:
{
  programs.aerospace = {
    enable = pkgs.stdenv.isDarwin;
    config = builtins.readFile ./aerospace.toml;
  };
}
