{ pkgs }:
{
  home.packages =
    with pkgs;
    pkgs.lib.optionals pkgs.stdenv.isLinux [
      swww
    ];

  # Wallpaper script
  xdg.configFile."swww/wallpaper.sh" = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      # Default wallpaper directory
      WALLPAPER_DIR="''${HOME}/Pictures/Wallpapers"
      CURRENT_WALLPAPER="''${XDG_STATE_HOME:-$HOME/.local/state}/swww/current"

      # Transition settings (Dracula-inspired)
      TRANSITION_TYPE="wipe"
      TRANSITION_DURATION="1"
      TRANSITION_FPS="60"
      TRANSITION_ANGLE="30"
      TRANSITION_BEZIER=".43,1.19,1,.4"

      set_wallpaper() {
        local wallpaper="$1"

        if [[ ! -f "$wallpaper" ]]; then
          echo "Error: Wallpaper not found: $wallpaper"
          exit 1
        fi

        swww img "$wallpaper" \
          --transition-type "$TRANSITION_TYPE" \
          --transition-duration "$TRANSITION_DURATION" \
          --transition-fps "$TRANSITION_FPS" \
          --transition-angle "$TRANSITION_ANGLE" \
          --transition-bezier "$TRANSITION_BEZIER"

        # Save current wallpaper path
        mkdir -p "$(dirname "$CURRENT_WALLPAPER")"
        echo "$wallpaper" > "$CURRENT_WALLPAPER"
      }

      random_wallpaper() {
        if [[ ! -d "$WALLPAPER_DIR" ]]; then
          echo "Wallpaper directory not found: $WALLPAPER_DIR"
          exit 1
        fi

        local wallpaper
        wallpaper=$(find "$WALLPAPER_DIR" -type f \( -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.webp" \) | shuf -n 1)

        if [[ -n "$wallpaper" ]]; then
          set_wallpaper "$wallpaper"
        else
          echo "No wallpapers found in $WALLPAPER_DIR"
          exit 1
        fi
      }

      case "$1" in
        set)
          if [[ -n "$2" ]]; then
            set_wallpaper "$2"
          else
            echo "Usage: $0 set <path-to-wallpaper>"
            exit 1
          fi
          ;;
        random)
          random_wallpaper
          ;;
        current)
          if [[ -f "$CURRENT_WALLPAPER" ]]; then
            cat "$CURRENT_WALLPAPER"
          else
            echo "No wallpaper set"
          fi
          ;;
        *)
          echo "Usage: $0 {set <path>|random|current}"
          echo ""
          echo "Commands:"
          echo "  set <path>  Set a specific wallpaper"
          echo "  random      Set a random wallpaper from $WALLPAPER_DIR"
          echo "  current     Show current wallpaper path"
          exit 1
          ;;
      esac
    '';
  };

  # Create wallpaper directory
  home.file."Pictures/Wallpapers/.keep".text = "";
}
