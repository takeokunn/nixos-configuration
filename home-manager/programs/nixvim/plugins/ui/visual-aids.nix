{ pkgs }:
{
  plugins.indent-blankline = {
    enable = true;
    settings = {
      indent = {
        char = "│";
      };
      scope = {
        enabled = true;
        show_start = true;
        show_end = false;
      };
    };
  };

  plugins.todo-comments = {
    enable = true;
    settings = {
      signs = true;
      keywords = {
        FIX = {
          icon = " ";
          color = "error";
          alt = [ "FIXME" "BUG" "FIXIT" "ISSUE" ];
        };
        TODO = {
          icon = " ";
          color = "info";
        };
        HACK = {
          icon = " ";
          color = "warning";
        };
        WARN = {
          icon = " ";
          color = "warning";
          alt = [ "WARNING" "XXX" ];
        };
        PERF = {
          icon = " ";
          color = "hint";
          alt = [ "OPTIM" "PERFORMANCE" "OPTIMIZE" ];
        };
        NOTE = {
          icon = " ";
          color = "hint";
          alt = [ "INFO" ];
        };
      };
      colors = {
        error = [ "DiagnosticError" "ErrorMsg" "#FF5555" ];
        warning = [ "DiagnosticWarn" "WarningMsg" "#FFB86C" ];
        info = [ "DiagnosticInfo" "#8BE9FD" ];
        hint = [ "DiagnosticHint" "#50FA7B" ];
        default = [ "Identifier" "#BD93F9" ];
      };
      highlight = {
        multiline = true;
        keyword = "wide";
        comments_only = true;
      };
    };
    lazyLoad.settings = {
      event = [ "BufReadPre" "BufNewFile" ];
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>ft";
      action = "<cmd>TodoTelescope<CR>";
      options.desc = "Search TODO comments";
    }
  ];
}
