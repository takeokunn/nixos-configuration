{ pkgs, sources }:
let
  plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in
[
  # {
  #   type = "lua";
  #   plugin = plugins.skkeleton;
  #   config = ''
  #     vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })
  #   '';
  # }
]
