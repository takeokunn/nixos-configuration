{ pkgs }:
{
  plugins.treesitter = {
    enable = true;
    settings = {
      highlight = {
        enable = true;
        additional_vim_regex_highlighting = false;
        # 大きなファイルではハイライトを無効化
        disable = ''
          function(lang, buf)
            local max_filesize = 100 * 1024
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
              return true
            end
          end
        '';
      };
      indent = {
        enable = true;
      };
      incremental_selection = {
        enable = true;
        keymaps = {
          init_selection = "<CR>";
          node_incremental = "<CR>";
          node_decremental = "<BS>";
          scope_incremental = false;
        };
      };
    };
    grammarPackages = pkgs.vimPlugins.nvim-treesitter.allGrammars;
  };
}
