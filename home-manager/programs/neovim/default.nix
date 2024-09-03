{ pkgs, neovim-nightly-overlay }: {
  programs.neovim = {
    enable = true;
    package = neovim-nightly-overlay.packages.${pkgs.system}.default;
    withNodeJs = false;
    withRuby = false;
    withPython3 = false;
    plugins = with pkgs.vimPlugins; [
      dracula-nvim
      lualine-nvim
      vim-markdown
      hop-nvim
    ];
  };

  xdg.configFile."nvim/init.lua".source = ./init.lua;
}
