{ pkgs }:
let vimdoc-ja = pkgs.callPackage ./nixpkgs/vimdoc-ja.nix { };
in with pkgs.vimPlugins; [
  vim-markdown
  {
    type = "lua";
    plugin = hop-nvim;
    config = ''
      require('hop').setup { }
      vim.keymap.set('n', 's', '<Cmd>HopChar1<CR>')
    '';
  }
  {
    type = "lua";
    plugin = gitsigns-nvim;
    config = ''
      require('gitsigns').setup()
    '';
  }
  {
    type = "lua";
    plugin = undotree;
    config = ''
      vim.g.undotree_WindowLayout = 3
      vim.api.nvim_create_user_command('UndotreeToggleAndFocus', ':UndotreeToggle | :UndotreeFocus', {})
      vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggleAndFocus)
    '';
  }
  {
    type = "lua";
    plugin = rainbow;
    config = ''
      vim.g.rainbow_active = 1
    '';
  }
  auto-pairs
  vim-bracketed-paste
  vimdoc-ja
]
