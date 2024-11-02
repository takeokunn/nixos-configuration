{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  programs = {
    actionlint.enable = true;
    nixfmt.enable = true;
    taplo.enable = true;
    jsonfmt.enable = true;
    yamlfmt.enable = true;
    fish_indent.enable = true;
    stylua.enable = true;
  };
}
