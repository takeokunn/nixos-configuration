{ pkgs }:
{
  plugins.noice = {
    enable = true;
    settings = {
      lsp = {
        override = {
          "vim.lsp.util.convert_input_to_markdown_lines" = true;
          "vim.lsp.util.stylize_markdown" = true;
          "cmp.entry.get_documentation" = true;
        };
      };
      presets = {
        bottom_search = true;
        command_palette = true;
        long_message_to_split = true;
        inc_rename = false;
        lsp_doc_border = false;
      };
    };
    lazyLoad.settings.event = "VeryLazy";
  };

  plugins.notify = {
    enable = true;
    settings = {
      background_colour = "#282a36";
      fps = 30;
      render = "default";
      stages = "fade_in_slide_out";
      timeout = 3000;
      top_down = false;
    };
  };

  plugins.fidget = {
    enable = true;
    settings = {
      notification = {
        window = {
          winblend = 0;
          border = "none";
          align = "bottom";
        };
      };
    };
  };
}
