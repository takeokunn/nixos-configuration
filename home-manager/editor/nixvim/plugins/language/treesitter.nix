{ pkgs }:
{
  plugins.treesitter.enable = true;
  # Native (nvim-treesitter main-branch) highlighting. `highlight.disable` here
  # is a `listOf str` filetype blocklist, not a raw-lua predicate — the modern
  # module does not support a size-based disable function, so the old large-file
  # guard is dropped rather than moved to the deprecated `settings.highlight.*`
  # legacy path (which warns and is a no-op on the main-branch package).
  plugins.treesitter.highlight.enable = true;
  plugins.treesitter.settings.indent.enable = true;
  plugins.treesitter.settings.incremental_selection.enable = true;
  plugins.treesitter.settings.incremental_selection.keymaps.init_selection = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_incremental = "<CR>";
  plugins.treesitter.settings.incremental_selection.keymaps.node_decremental = "<BS>";
  plugins.treesitter.settings.incremental_selection.keymaps.scope_incremental = false;
  plugins.treesitter.grammarPackages = pkgs.vimPlugins.nvim-treesitter.allGrammars;
}
