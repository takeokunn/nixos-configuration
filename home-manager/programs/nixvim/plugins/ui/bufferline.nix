{ pkgs }:
{
  plugins.bufferline = {
    enable = true;

    settings = {
      options = {
        themable = true;

        diagnostics = "nvim_lsp";
        diagnostics_indicator = ''
          function(count, level, diagnostics_dict, context)
            local icon = level:match("error") and " " or " "
            return " " .. icon .. count
          end
        '';

        separator_style = "slant";

        always_show_bufferline = true;

        offsets = [
          {
            filetype = "oil";
            text = "File Explorer";
            text_align = "center";
            separator = true;
          }
        ];

        buffer_close_icon = "󰅖";
        modified_icon = "●";
        close_icon = "";
        left_trunc_marker = "";
        right_trunc_marker = "";

        show_buffer_icons = true;
        show_buffer_close_icons = true;
        show_close_icon = true;
        show_tab_indicators = true;

        persist_buffer_sort = true;
        enforce_regular_tabs = false;

        max_name_length = 18;
        max_prefix_length = 15;
        tab_size = 18;
      };
    };

    lazyLoad.settings = {
      event = [
        "BufReadPost"
        "BufNewFile"
      ];
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<Tab>";
      action = "<cmd>BufferLineCycleNext<CR>";
      options = {
        desc = "次のバッファへ移動";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<S-Tab>";
      action = "<cmd>BufferLineCyclePrev<CR>";
      options = {
        desc = "前のバッファへ移動";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<leader>bp";
      action = "<cmd>BufferLineTogglePin<CR>";
      options = {
        desc = "バッファをピン留め";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<leader>bc";
      action = "<cmd>bd<CR>";
      options = {
        desc = "現在のバッファを閉じる";
        silent = true;
      };
    }
  ];
}
