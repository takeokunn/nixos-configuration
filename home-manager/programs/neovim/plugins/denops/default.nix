{ pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in with pkgs.vimPlugins; [
  denops-vim
  plugins.denops-helloworld
  {
    type = "lua";
    plugin = plugins.skkeleton;
    config = ''
      vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })
    '';
  }
]
