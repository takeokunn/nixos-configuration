{ pkgs }:
with pkgs.vimPlugins;
[
  {
    type = "lua";
    plugin = telescope-nvim;
    config = ''
      require('telescope').setup {
        defaults = {
          mappings = {
            i = { ["<esc>"] = actions.close },
            n = { ["q"] = actions.close }
          }
        },
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
      vim.keymap.set('n', '<leader>f', builtin.git_files)
      vim.keymap.set('n', '<leader>o', builtin.current_buffer_fuzzy_find)
      vim.keymap.set('n', '<leader>g', builtin.live_grep)
      vim.keymap.set('n', '<leader>b', builtin.buffers)
      vim.keymap.set('n', '<leader>h', builtin.help_tags)
      vim.keymap.set('n', '<leader>r', builtin.registers)
    '';
  }
  telescope-ui-select-nvim
  telescope-file-browser-nvim
  telescope-fzf-native-nvim
]
