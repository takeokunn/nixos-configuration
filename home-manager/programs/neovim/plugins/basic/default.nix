{ pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources; };
in with pkgs.vimPlugins; [
  vim-markdown
  {
    type = "lua";
    plugin = hop-nvim;
    config = ''
      require('hop').setup { }
      -- vim.keymap.set('n', 's', '<Cmd>HopChar1<CR>')
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
  plugins.vimdoc-ja
  {
    type = "lua";
    plugin = vim-sandwich;
    config = ''
      vim.call('operator#sandwich#set', 'add', 'char', 'skip_space', 1)
    '';
  }
  {
    type = "lua";
    plugin = plugins.vim-fern;
    config = ''
      vim.keymap.set('n', '<leader>e', '<cmd>Fern . -drawer<cr>')
    '';
  }
  {
    type = "lua";
    plugin = telescope-nvim;
    config = ''
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.git_files, { desc = 'Telescope find git files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
    '';
  }
  {
    type = "lua";
    plugin = nvim-lspconfig;
    config = ''
      local lspconfig = require('lspconfig')
      if vim.fn.executable('nil') == 1 then
        lspconfig.nil_ls.setup({
                  ['nil'] = {
                    formatting = {
                      command = { 'nixfmt' },
                    },
                  }
            })
      end
    '';
  }
]
