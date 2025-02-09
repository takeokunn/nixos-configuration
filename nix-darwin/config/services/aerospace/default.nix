{
  services.aerospace = {
    enable = false;
    settings = {
      start-at-login = false;
      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;
      accordion-padding = 0;
      on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
      exec-on-workspace-change = [
        "/bin/bash"
        "-c"
        "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(aerospace list-workspaces --focused)"
      ];

      gaps = {
        inner = {
          horizontal = 0;
          vertical = 0;
        };
        outer = {
          left = 0;
          bottom = 0;
          top = 0;
          right = 0;
        };
      };

      mode = {
        main = {
          binding = {
            alt-h = "focus left";
            alt-l = "focus right";

            alt-shift-h = "move left";
            alt-shift-l = "move right";

            alt-shift-space = "layout floating tiling";

            alt-1 = "workspace 1";
            alt-2 = "workspace 2";
            alt-3 = "workspace 3";
            alt-4 = "workspace 4";
            alt-5 = "workspace 5";
            alt-6 = "workspace 6";
            alt-7 = "workspace 7";
            alt-8 = "workspace 8";
            alt-9 = "workspace 9";
            alt-0 = "workspace 10";

            alt-shift-1 = [
              "move-node-to-workspace 1"
              "workspace 1"
            ];
            alt-shift-2 = [
              "move-node-to-workspace 2"
              "workspace 2"
            ];
            alt-shift-3 = [
              "move-node-to-workspace 3"
              "workspace 3"
            ];
            alt-shift-4 = [
              "move-node-to-workspace 4"
              "workspace 4"
            ];
            alt-shift-5 = [
              "move-node-to-workspace 5"
              "workspace 5"
            ];
            alt-shift-6 = [
              "move-node-to-workspace 6"
              "workspace 6"
            ];
            alt-shift-7 = [
              "move-node-to-workspace 7"
              "workspace 7"
            ];
            alt-shift-8 = [
              "move-node-to-workspace 8"
              "workspace 8"
            ];
            alt-shift-9 = [
              "move-node-to-workspace 9"
              "workspace 9"
            ];
            alt-shift-0 = [
              "move-node-to-workspace 10"
              "workspace 10"
            ];

            alt-r = "mode resize";
          };
        };

        resize = {
          binding = {
            h = "resize width -50";
            j = "resize height +50";
            k = "resize height -50";
            l = "resize width +50";
            enter = "mode main";
            esc = "mode main";
          };
        };
      };

      workspace-to-monitor-force-assignment = {
        "1" = "main";
        "2" = "main";
        "3" = "main";
        "4" = "main";
        "5" = "secondary";
        "6" = "secondary";
        "7" = "secondary";
        "8" = "secondary";
        "9" = "secondary";
        "10" = "main";
      };

      on-window-detected = [
        {
          "if".app-id = "com.google.Chrome";
          run = [
            "layout floating"
            "move-node-to-workspace 1"
          ];
        }
        {
          "if".app-id = "com.apple.Terminal";
          run = [ "move-node-to-workspace 2" ];
        }
        {
          "if".app-id = "com.raphaelamorim.rio";
          run = [ "move-node-to-workspace 2" ];
        }
        {
          "if".app-id = "net.kovidgoyal.kitty";
          run = [ "move-node-to-workspace 2" ];
        }
        {
          "if".app-id = "org.gnu.Emacs";
          run = [ "move-node-to-workspace 3" ];
        }
        {
          "if".app-id = "com.jgraph.drawio.desktop";
          run = [ "move-node-to-workspace 4" ];
        }
        {
          "if".app-id = "io.flutterflow.prod.mac";
          run = [ "move-node-to-workspace 4" ];
        }
        {
          "if".app-id = "com.obsproject.obs-studio";
          run = [ "move-node-to-workspace 7" ];
        }
        {
          "if".app-id = "com.github.tattn.VCam";
          run = [ "move-node-to-workspace 7" ];
        }
        {
          "if".app-id = "com.hnc.Discord";
          run = [ "move-node-to-workspace 8" ];
        }
        {
          "if".app-id = "com.openai.chat";
          run = [ "move-node-to-workspace 9" ];
        }
        {
          "if".app-id = "com.tinyspeck.slackmacgap";
          run = [ "move-node-to-workspace 10" ];
        }
      ];
    };
  };
}
