{ emacsLib }:
let
  emacsScratchpadToggle = emacsLib.mkScratchpadToggle {
    windowManager = "aerospace";
  };

  workspaceApps = {
    "1" = [ "com.google.Chrome" ];
    "3" = [ "org.gnu.Emacs" ];
    "4" = [
      "com.jgraph.drawio.desktop"
      "com.lambdalisue.Arto"
      "com.sequel-ace.sequel-ace"
      "at.eggerapps.Postico"
    ];
    "6" = [ "com.efoo.mediator" ];
    "7" = [ "com.slite.desktop" ];
    "8" = [ "com.clickup.desktop-app" ];
    "9" = [
      "com.google.Chrome.app.caidcmannjgahlnbpmidmiecjcoiiigg"
      "com.hnc.Discord"
      "jp.naver.line.mac"
    ];
    "10" = [ "com.tinyspeck.slackmacgap" ];
  };

  floatingApps = [
    "com.google.Chrome"
    "pl.maketheweb.cleanshotx"
    "io.github.keycastr"
    "ai.amical.desktop"
  ];
in
{
  services.aerospace.enable = true;
  services.aerospace.settings = {
    enable-normalization-flatten-containers = true;
    enable-normalization-opposite-orientation-for-nested-containers = true;
    accordion-padding = 0;
    on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
    exec-on-workspace-change = [
      "/bin/bash"
      "-c"
      "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(/run/current-system/sw/bin/aerospace list-workspaces --focused)"
    ];

    gaps = {
      inner = {
        horizontal = 0;
        vertical = 0;
      };
      outer = {
        left = 0;
        bottom = 0;
        top = 10;
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

          # Keep in sync with the niri Mod+I binding.
          alt-i = "exec-and-forget ${emacsScratchpadToggle}";
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
      "6" = "main";
      "7" = "main";
      "8" = "main";
      "9" = "main";
      "10" = "main";
    };

    # AeroSpace stops at the first matching callback unless check-further-callbacks
    # is set, so the floating rules must precede the move rules for an app in both.
    on-window-detected = [
      {
        "if".app-id = "net.kovidgoyal.kitty";
        "if".window-title-regex-substring = "FloatingEmacs";
        run = [ "layout floating" ];
      }
    ]
    ++ map (app: {
      "if".app-id = app;
      check-further-callbacks = true;
      run = [ "layout floating" ];
    }) floatingApps
    ++ builtins.concatMap (
      ws:
      map (app: {
        "if".app-id = app;
        run = [ "move-node-to-workspace ${ws}" ];
      }) workspaceApps.${ws}
    ) (builtins.attrNames workspaceApps);
  };
}
