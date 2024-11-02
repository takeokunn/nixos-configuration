{ pkgs }:
with pkgs.vimPlugins;
[
  {
    type = "lua";
    plugin = telescope-nvim;
    config = ''
      require('telescope').setup {
        extensions = {
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          }
        }
      }

      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.git_files, { desc = 'Telescope find git files' })
      vim.keymap.set('n', '<leader>fo', builtin.current_buffer_fuzzy_find, { desc = 'Telescope buffer fuzzy find' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
    '';
  }
  telescope-ui-select-nvim
  telescope-file-browser-nvim
  telescope-fzf-native-nvim
]
