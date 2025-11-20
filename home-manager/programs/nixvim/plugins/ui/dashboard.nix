{ pkgs }:
{
  plugins.dashboard = {
    enable = true;
    settings = {
      theme = "doom";
      config = {
        header = [
          ""
          "███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗"
          "████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║"
          "██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║"
          "██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║"
          "██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║"
          "╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝╚═╝     ╚═╝"
          ""
        ];
        center = [
          {
            icon = "  ";
            icon_hl = "DashboardIcon";
            desc = "Find Git Files";
            desc_hl = "DashboardDesc";
            key = "f";
            key_hl = "DashboardKey";
            key_format = " %s";
            action = "Telescope git_files";
          }
          {
            icon = "  ";
            icon_hl = "DashboardIcon";
            desc = "Live Grep";
            desc_hl = "DashboardDesc";
            key = "g";
            key_hl = "DashboardKey";
            key_format = " %s";
            action = "Telescope live_grep";
          }
          {
            icon = "  ";
            icon_hl = "DashboardIcon";
            desc = "Recent Files";
            desc_hl = "DashboardDesc";
            key = "r";
            key_hl = "DashboardKey";
            key_format = " %s";
            action = "Telescope oldfiles";
          }
          {
            icon = "  ";
            icon_hl = "DashboardIcon";
            desc = "Buffers";
            desc_hl = "DashboardDesc";
            key = "b";
            key_hl = "DashboardKey";
            key_format = " %s";
            action = "Telescope buffers";
          }
          {
            icon = "  ";
            icon_hl = "DashboardIcon";
            desc = "Help Tags";
            desc_hl = "DashboardDesc";
            key = "h";
            key_hl = "DashboardKey";
            key_format = " %s";
            action = "Telescope help_tags";
          }
        ];
        footer = [
          ""
          "🚀 Happy Coding with NixVim!"
        ];
      };
    };
  };
}
