{ pkgs, neovim-nightly-overlay }:
let
  denops-helloworld = pkgs.callPackage ./packages/denops-helloworld.nix { };
  skkeleton = pkgs.callPackage ./packages/skkeleton.nix { };
  vimdoc-ja = pkgs.callPackage ./packages/vimdoc-ja.nix { };
in {
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
      gitsigns-nvim
      undotree
      editorconfig-vim
      rainbow
      auto-pairs
      vim-bracketed-paste
      denops-vim
      denops-helloworld
      skkeleton
      vimdoc-ja
    ];

    coc.enable = true;
  };

  xdg.configFile."nvim/init.lua".source = ./init.lua;
}
