{
  programs.bottom = {
    enable = true;
    settings = {
      row = [
        {
          child = [
            {
              ratio = 2;
              type = "cpu";
            }
            { type = "mem"; }
          ];
        }
        {
          child = [
            {
              ratio = 2;
              type = "net";
            }
            {
              ratio = 2;
              type = "disk";
            }
            { type = "temp"; }
          ];
        }
        {
          ratio = 3;
          child = [
            {
              default = true;
              type = "proc";
            }
          ];
        }
      ];
      colors = {
        avg_cpu_color = "Red";
        border_color = "White";
        graph_color = "Gray";
        highlighted_border_color = "LightMagenta";
        selected_bg_color = "Magenta";
        selected_text_color = "Black";
        table_header_color = "Blue";
        text_color = "White";
        widget_title_color = "Cyan";
      };
      disk_filter = {
        is_list_ignored = true;
        list = [ "/dev/loop\\d+" ];
        regex = true;
      };
      flags = {
        basic = false;
        case_sensitive = false;
        dot_marker = false;
        group_processes = true;
        hide_table_gap = true;
        rate = 700;
      };
    };
  };
}
