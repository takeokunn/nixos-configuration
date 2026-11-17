{ lib, emacsLib, ... }:
let
  # Workspace numbers and app placement match the niri layout on X13Gen2.
  workspaces = map toString (lib.range 1 10);
  workspaceKey = ws: if ws == "10" then "0" else ws;

  emacsScratchpadToggle = emacsLib.mkScratchpadToggle {
    windowManager = "aerospace";
  };

  workspaceApps = {
    "1" = [ "com.google.Chrome" ];
    "3" = [ "org.gnu.Emacs" ];
    "4" = [
      "com.lambdalisue.Arto"
      "com.sequel-ace.sequel-ace"
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
  ];
in
{
  services.aerospace.enable = true;
  services.aerospace.settings = {
    config-version = 2;
    persistent-workspaces = workspaces;
    enable-normalization-flatten-containers = true;
    enable-normalization-opposite-orientation-for-nested-containers = true;
    accordion-padding = 0;
    on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
    # launchd starts AeroSpace with PATH=/usr/bin:/bin:/usr/sbin:/sbin, so the
    # home-manager sketchybar must be reached through the per-user profile.
    exec-on-workspace-change = [
      "/bin/bash"
      "-c"
      "u=$(id -un); USER=$u /etc/profiles/per-user/$u/bin/sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(/run/current-system/sw/bin/aerospace list-workspaces --focused)"
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

          alt-r = "mode resize";

          # Keep in sync with the niri Mod+I binding.
          alt-i = "exec-and-forget ${emacsScratchpadToggle}";
        }
        // lib.listToAttrs (
          lib.concatMap (ws: [
            (lib.nameValuePair "alt-${workspaceKey ws}" "workspace ${ws}")
            (lib.nameValuePair "alt-shift-${workspaceKey ws}" [
              "move-node-to-workspace ${ws}"
              "workspace ${ws}"
            ])
          ]) workspaces
        );
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

    workspace-to-monitor-force-assignment = lib.genAttrs workspaces (_: "main") // {
      "5" = "secondary";
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
