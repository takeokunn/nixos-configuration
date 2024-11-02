{ pkgs, sources }:
let
  plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in
with pkgs.vimPlugins;
[
  {
    type = "lua";
    plugin = gitsigns-nvim;
    config = ''
      require('gitsigns').setup()
    '';
  }
  plugins.vim-gin
]
