{ pkgs, sources }:
let
  plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in
[
  {
    type = "lua";
    plugin = plugins.skkeleton;
    config = ''
      vim.fn['skkeleton#config']({
        eggLikeNewline = true,
        keepState = true,
        sources = { "skk_server" }
      })
      vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })
    '';
  }
]
