{ pkgs, sources }:
let
  plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in
with pkgs.vimPlugins;
[
  # cursor
  {
    type = "lua";
    plugin = hop-nvim;
    config = ''
      require('hop').setup { }
      vim.keymap.set('n', 'f', '<Cmd>HopChar1<CR>')
    '';
  }

  # clipboard
  vim-bracketed-paste

  # file tree
  {
    type = "lua";
    plugin = plugins.vim-fern;
    config = ''
      vim.keymap.set('n', '<leader>e', '<cmd>Fern . -drawer<cr>')
    '';
  }

  # history tree
  {
    type = "lua";
    plugin = undotree;
    config = ''
      vim.g.undotree_WindowLayout = 3
      vim.api.nvim_create_user_command('UndotreeToggleAndFocus', ':UndotreeToggle | :UndotreeFocus', {})
      vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggleAndFocus)
    '';
  }

  # indent
  vim-textobj-entire

  # docs
  plugins.vimdoc-ja

  # utils
  auto-pairs
  {
    type = "lua";
    plugin = rainbow;
    config = ''
      vim.g.rainbow_active = 1
    '';
  }
  {
    type = "lua";
    plugin = vim-sandwich;
    config = ''
      vim.call('operator#sandwich#set', 'add', 'char', 'skip_space', 1)
    '';
  }
]
