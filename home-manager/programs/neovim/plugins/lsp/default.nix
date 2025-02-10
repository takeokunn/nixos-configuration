{ pkgs }:
with pkgs.vimPlugins;
[
  {
    type = "lua";
    plugin = nvim-cmp;
    config = ''
      local cmp = require("cmp")

      cmp.setup({
        snippet = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
          end,
        },
        sources = {
          { name = "nvim_lsp" },
          { name = "path" },
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-p>"] = cmp.mapping.select_prev_item(),
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ['<C-l>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm { select = true },
        }),
        experimental = {
          ghost_text = true,
        },
      })
    '';
  }
  {
    type = "lua";
    plugin = nvim-lspconfig;
    config = ''
      local lspconfig = require('lspconfig')

      vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', { silent = true, buffer = buffer })

      if vim.fn.executable('nil') == 1 then
        lspconfig.nil_ls.setup {
          settings = {
            ['nil'] = {
              formatting = {
                command = { 'nixfmt' }
              }
            }
          }
        }
      end

      if vim.fn.executable('typescript-language-server') == 1 then
        lspconfig.ts_ls.setup { }
      end

      if vim.fn.executable('intelephense') == 1 then
        lspconfig.intelephense.setup { }
      end
    '';
  }
  {
    type = "lua";
    plugin = lspsaga-nvim;
    config = ''
      require('lspsaga').setup({
        code_action = {
          extend_gitsigns = true,
        },
        finder = {
          max_height = 0.7,
          left_width = 0.3,
          right_width = 0.6,
          keys = {
            shuttle = "<Space>w",
            toggle_or_open = "<CR>"
          }
        },
        lightbulb = {
          enable = false,
        }
      })

      vim.keymap.set('n', 'K', '<cmd>Lspsaga hover_doc<CR>')
      vim.keymap.set({ 'n', 'i' }, '<S-M-r>', "<cmd>Lspsaga rename<CR>", opts)
      vim.keymap.set('n', '<M-d>', "<cmd>Lspsaga finder def+ref<CR>", opts)
      vim.keymap.set('n', '<M-r>', "<cmd>Lspsaga peek_definition<CR>", opts)
      vim.keymap.set('n', '<M-j>', "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
      vim.keymap.set('n', '<M-k>', "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
    '';
  }
]
