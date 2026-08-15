{ pkgs }:
{
  plugins.treesitter.enable = true;
  plugins.treesitter.highlight.enable = true;
  plugins.treesitter.settings.indent.enable = true;
  plugins.treesitter.settings.incremental_selection.enable = true;
  plugins.treesitter.settings.incremental_selection.keymaps.init_selection = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_incremental = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_decremental = "<BS>";
  plugins.treesitter.settings.incremental_selection.keymaps.scope_incremental = false;
  plugins.treesitter.grammarPackages = pkgs.vimPlugins.nvim-treesitter.allGrammars;
}
