{ pkgs, sources }:
let denops = pkgs.callPackage ./plugins.nix { inherit sources; };
in with pkgs.vimPlugins; [
  denops-vim
  denops.denops-helloworld
  {
    type = "lua";
    plugin = denops.skkeleton;
    config = ''
      vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })
    '';
  }
]
