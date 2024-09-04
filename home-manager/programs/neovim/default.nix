{ pkgs, neovim-nightly-overlay }: {
  programs.neovim = {
    enable = true;
    package = neovim-nightly-overlay.packages.${pkgs.system}.default;
    withNodeJs = false;
    withRuby = false;
    withPython3 = false;
    plugins = import ./plugins { inherit pkgs; };

    coc.enable = true;

    extraLuaConfig = builtins.readFile ./init.lua;
  };
}
