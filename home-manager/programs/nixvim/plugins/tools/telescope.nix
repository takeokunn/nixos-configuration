{
  plugins.telescope = {
    enable = true;
    lazyLoad.settings = {
      cmd = "Telescope";
    };
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
          i = {
            "<Esc>" = "close";
          };
          n = {
            "<Esc>" = "close";
          };
        };
      };
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>ff";
      action = "<cmd>Telescope git_files<CR>";
      options.desc = "Find Git Files";
    }
    {
      mode = "n";
      key = "<leader>fo";
      action = "<cmd>Telescope current_buffer_fuzzy_find<CR>";
      options.desc = "Find in Current Buffer";
    }
    {
      mode = "n";
      key = "<leader>fg";
      action = "<cmd>Telescope live_grep<CR>";
      options.desc = "Live Grep";
    }
    {
      mode = "n";
      key = "<leader>fb";
      action = "<cmd>Telescope buffers<CR>";
      options.desc = "Find Buffers";
    }
    {
      mode = "n";
      key = "<leader>fh";
      action = "<cmd>Telescope help_tags<CR>";
      options.desc = "Find Help Tags";
    }
    {
      mode = "n";
      key = "<leader>fr";
      action = "<cmd>Telescope registers<CR>";
      options.desc = "Find Registers";
    }
  ];
}
