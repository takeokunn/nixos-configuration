{ pkgs, neovim-nightly-overlay, sources }: {
  programs.neovim = {
    enable = true;
    package = neovim-nightly-overlay.packages.${pkgs.system}.default;
    withNodeJs = false;
    withRuby = false;
    withPython3 = false;
    plugins = import ./plugins { inherit pkgs sources; };

    extraLuaConfig = builtins.readFile ./init.lua;
  };
}
