{ pkgs }:
{
  enable = true;
  defaultEditor = true;

  colorschemes.dracula.enable = true;

  enableMan = true;

  editorconfig.enable = true;

  clipboard.providers = {
    pbcopy.enable = pkgs.stdenv.isDarwin;
    wl-copy.enable = pkgs.stdenv.isLinux;
  };
}
