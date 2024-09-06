{ pkgs, neovim-nightly-overlay, sources }: {
  programs.neovim = {
    enable = true;
    package = neovim-nightly-overlay.packages.${pkgs.system}.default;
    withNodeJs = false;
    withRuby = false;
    withPython3 = false;
    plugins = import ./plugins { inherit pkgs sources; };

    coc.enable = true;

    extraLuaConfig = builtins.readFile ./init.lua;
  };
}
