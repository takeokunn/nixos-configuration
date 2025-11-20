{ pkgs, customPackages }:
{
  plugins.oil = {
    enable = true;
    settings = {
      default_file_explorer = true;
      columns = [ "icon" ];

      buf_options = {
        buflisted = false;
        bufhidden = "hide";
      };

      win_options = {
        wrap = false;
        signcolumn = "no";
        cursorcolumn = false;
        foldcolumn = "0";
        spell = false;
        list = false;
        conceallevel = 3;
        concealcursor = "nvic";
      };

      delete_to_trash = true;
      skip_confirm_for_simple_edits = false;
      prompt_save_on_select_new_entry = true;

      lsp_file_methods = {
        enabled = true;
        timeout_ms = 1000;
        autosave_changes = false;
      };

      # Git統合（ファイル操作を自動的にgit add/mv/rmで管理）
      git = {
        add = ''
          function(path)
            return true
          end
        '';
        mv = ''
          function(src_path, dest_path)
            return true
          end
        '';
        rm = ''
          function(path)
            return true
          end
        '';
      };

      view_options = {
        show_hidden = false;
        natural_order = "fast";
        # gitignoreされたファイルを非表示、Git管理されたdotfileは表示
        is_hidden_file = ''
          function(name, bufnr)
            local dir = require("oil").get_current_dir(bufnr)
            local is_dotfile = vim.startswith(name, ".") and name ~= ".."
            if not dir then
              return is_dotfile
            end
            if is_dotfile then
              return not git_status[dir].tracked[name]
            else
              return git_status[dir].ignored[name]
            end
          end
        '';
      };

      use_default_keymaps = true;

      float = {
        padding = 2;
        max_width = 50;
        max_height = 0;
        border = "rounded";
        win_options = {
          winblend = 0;
        };
        override = ''
          function(conf)
            local screen_w = vim.o.columns
            local screen_h = vim.o.lines
            local width = 50
            conf.col = screen_w - width - 2
            conf.row = 0
            conf.width = width
            conf.height = screen_h - 2
            return conf
          end
        '';
      };
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>e";
      action = "<cmd>lua require('oil').toggle_float()<cr>";
      options = {
        desc = "Toggle Oil file explorer";
        silent = true;
      };
    }
  ];
}
