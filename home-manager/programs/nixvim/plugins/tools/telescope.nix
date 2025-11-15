{ pkgs }:
{
  plugins.telescope = {
    enable = true;
    extensions = {
      fzf-native = {
        enable = true;
        settings = {
          fuzzy = true;
          override_generic_sorter = true;
          override_file_sorter = true;
          case_mode = "smart_case";
        };
      };
      ui-select = {
        enable = true;
      };
      file-browser = {
        enable = true;
      };
    };
    settings = {
      defaults = {
        mappings = {
          n = {
            q = "close";
          };
        };
      };
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>f";
      action = "<cmd>Telescope git_files<CR>";
    }
    {
      mode = "n";
      key = "<leader>o";
      action = "<cmd>Telescope current_buffer_fuzzy_find<CR>";
    }
    {
      mode = "n";
      key = "<leader>g";
      action = "<cmd>Telescope live_grep<CR>";
    }
    {
      mode = "n";
      key = "<leader>b";
      action = "<cmd>Telescope buffers<CR>";
    }
    {
      mode = "n";
      key = "<leader>h";
      action = "<cmd>Telescope help_tags<CR>";
    }
    {
      mode = "n";
      key = "<leader>r";
      action = "<cmd>Telescope registers<CR>";
    }
  ];
}
