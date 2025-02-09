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
    '';
  }
]
