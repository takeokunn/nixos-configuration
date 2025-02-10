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
      vim.keymap.set('n', '<leader>f', builtin.git_files, { desc = 'Telescope find git files' })
      vim.keymap.set('n', '<leader>o', builtin.current_buffer_fuzzy_find, { desc = 'Telescope buffer fuzzy find' })
      vim.keymap.set('n', '<leader>g', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>b', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>h', builtin.help_tags, { desc = 'Telescope help tags' })
    '';
  }
  telescope-ui-select-nvim
  telescope-file-browser-nvim
  telescope-fzf-native-nvim
]
