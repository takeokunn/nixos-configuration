{ pkgs }:
let
  denops-helloworld = pkgs.callPackage ./nixpkgs/denops-helloworld.nix { };
  skkeleton = pkgs.callPackage ./nixpkgs/skkeleton.nix { };
in with pkgs.vimPlugins; [
  denops-vim
  denops-helloworld
  {
    type = "lua";
    plugin = skkeleton;
    config = ''
      vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })
    '';
  }
]
