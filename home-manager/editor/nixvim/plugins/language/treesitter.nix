{ pkgs }:
{
  plugins.treesitter.enable = true;
  plugins.treesitter.highlight.disable.__raw = ''
    function(lang, buf)
      local max_filesize = 100 * 1024
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end
  '';
  plugins.treesitter.settings.indent.enable = true;
  plugins.treesitter.settings.incremental_selection.enable = true;
  plugins.treesitter.settings.incremental_selection.keymaps.init_selection = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_incremental = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_decremental = "<BS>";
  plugins.treesitter.settings.incremental_selection.keymaps.scope_incremental = false;
  plugins.treesitter.grammarPackages = pkgs.vimPlugins.nvim-treesitter.allGrammars;
}
