{ pkgs, sources }:
let
  plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in
[
  plugins.ddc-source-file
  plugins.ddc-converter_remove_overlap
  plugins.ddc-matcher_head
  plugins.ddc-matcher_length
  plugins.ddc-sorter_rank
  plugins.ddc-source-around
  plugins.ddc-source-cmdline
  plugins.ddc-source-cmdline-history
  plugins.ddc-source-codeium
  plugins.ddc-source-input
  plugins.ddc-source-line
  plugins.ddc-source-lsp
  plugins.ddc-source-rg
  plugins.ddc-source-shell-native
  plugins.ddc-ui-pum
  plugins.ddc
  plugins.ddc-shell-history
  plugins.ddc-buffer
  plugins.ddc-fuzzy
  plugins.ddc-source-lsp-setup
  plugins.ddc-source-nvim-lua
]
