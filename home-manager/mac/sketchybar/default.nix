{ pkgs, ... }:
let
  colors = {
    background = "0xff282a36";
    backgroundTransparent = "0xcc282a36";
    currentLine = "0xff44475a";
    foreground = "0xfff8f8f2";
    comment = "0xff6272a4";
    cyan = "0xff8be9fd";
    green = "0xff50fa7b";
    orange = "0xffffb86c";
    pink = "0xffff79c6";
    purple = "0xffbd93f9";
    red = "0xffff5555";
    yellow = "0xfff1fa8c";
    transparent = "0x00000000";
    black = "0xff21222c";
  };

  # 各space itemが自分自身の状態のみを更新（イベント駆動 + ポーリング両対応）
  aerospacePlugin = pkgs.writeShellScript "aerospace.sh" ''
    AEROSPACE="/run/current-system/sw/bin/aerospace"

    SID=$(echo "$NAME" | sed 's/space\.//')

    # フォーカス中のワークスペースを取得（イベント時は環境変数、ポーリング時はコマンド実行）
    if [ -n "$FOCUSED_WORKSPACE" ]; then
      FOCUSED="$FOCUSED_WORKSPACE"
    else
      FOCUSED=$("$AEROSPACE" list-workspaces --focused 2>/dev/null || echo "1")
    fi

    if [ "$SID" = "$FOCUSED" ]; then
      sketchybar --set "$NAME" \
        background.color=${colors.purple} \
        icon.color=${colors.background}
    else
      WINDOWS=$("$AEROSPACE" list-windows --workspace "$SID" 2>/dev/null | wc -l | tr -d ' ')
      if [ "$WINDOWS" -gt 0 ]; then
        sketchybar --set "$NAME" \
          background.color=${colors.transparent} \
          icon.color=${colors.foreground}
      else
        sketchybar --set "$NAME" \
          background.color=${colors.transparent} \
          icon.color=${colors.comment}
      fi
    fi
  '';

  cpuPlugin = pkgs.writeShellScript "cpu.sh" ''
    CPU=$(top -l 1 -n 0 | grep "CPU usage" | awk '{print $3}' | tr -d '%')
    sketchybar --set "$NAME" label="$CPU%"
  '';

  memoryPlugin = pkgs.writeShellScript "memory.sh" ''
    MEMORY=$(memory_pressure | grep "System-wide memory free percentage:" | awk '{print 100-$5}' | tr -d '%')
    sketchybar --set "$NAME" label="$MEMORY%"
  '';

  volumePlugin = pkgs.writeShellScript "volume.sh" ''
    VOLUME=$(osascript -e 'output volume of (get volume settings)')
    MUTED=$(osascript -e 'output muted of (get volume settings)')

    if [ "$MUTED" = "true" ]; then
      ICON="󰖁"
      sketchybar --set "$NAME" icon="$ICON" label="mute"
    else
      if [ "$VOLUME" -ge 70 ]; then
        ICON="󰕾"
      elif [ "$VOLUME" -ge 30 ]; then
        ICON="󰖀"
      else
        ICON="󰕿"
      fi
      sketchybar --set "$NAME" icon="$ICON" label="$VOLUME%"
    fi
  '';

  wifiPlugin = pkgs.writeShellScript "wifi.sh" ''
    SSID=$(ipconfig getsummary en0 2>/dev/null | grep "^  SSID" | awk -F ' : ' '{print $2}')

    if [ -z "$SSID" ]; then
      sketchybar --set "$NAME" icon.color=${colors.red} label="Off"
    else
      sketchybar --set "$NAME" icon.color=${colors.cyan} label="$SSID"
    fi
  '';

  datePlugin = pkgs.writeShellScript "date.sh" ''
    sketchybar --set "$NAME" label="$(date '+%m/%d %a')"
  '';

  timePlugin = pkgs.writeShellScript "time.sh" ''
    sketchybar --set "$NAME" label="$(date '+%H:%M')"
  '';

  batteryPlugin = pkgs.writeShellScript "power.sh" ''
    PERCENTAGE=$(pmset -g batt | grep -Eo "[0-9]+%" | cut -d% -f1)
    CHARGING=$(pmset -g batt | grep 'AC Power')

    if [ -z "$PERCENTAGE" ]; then
      exit 0
    fi

    case ''${PERCENTAGE} in
      100)        ICON="󰁹" ;;
      9[0-9])     ICON="󰂂" ;;
      8[0-9])     ICON="󰂁" ;;
      7[0-9])     ICON="󰂀" ;;
      6[0-9])     ICON="󰁿" ;;
      5[0-9])     ICON="󰁾" ;;
      4[0-9])     ICON="󰁽" ;;
      3[0-9])     ICON="󰁼" ;;
      2[0-9])     ICON="󰁻" ;;
      1[0-9])     ICON="󰁺" ;;
      *)          ICON="󰂎" ;;
    esac

    if [ -n "$CHARGING" ]; then
      ICON="󰂄"
      COLOR="${colors.green}"
    elif [ "$PERCENTAGE" -le 20 ]; then
      COLOR="${colors.red}"
    else
      COLOR="${colors.green}"
    fi

    sketchybar --set power_icon icon="$ICON" icon.color="$COLOR" \
                --set "$NAME" label="$PERCENTAGE%"
  '';

  frontAppPlugin = pkgs.writeShellScript "front_app.sh" ''
    if [ "$SENDER" = "front_app_switched" ]; then
      sketchybar --set "$NAME" label="$INFO"
    fi
  '';

  # Sleep-prevention display plugin script: reflects system-wide disablesleep state.
  # `pmset -g`'s "System-wide power settings:" block prints the setter keyword
  # "disablesleep" as "SleepDisabled" -- the getter and setter names differ.
  sleepPreventPlugin = pkgs.writeShellScript "sleep_prevent.sh" ''
    STATE=$(pmset -g | awk '/SleepDisabled/ {print $2}')

    if [ "$STATE" = "1" ]; then
      sketchybar --set "$NAME" icon.color=${colors.orange}
    else
      sketchybar --set "$NAME" icon.color=${colors.comment}
    fi
  '';

  # Sleep-prevention toggle click script: flips system-wide sleep via `sudo pmset -a
  # disablesleep`. Requires the NOPASSWD sudoers grant in nix-darwin/config/security.nix
  # (scoped to exactly these two invocations), since this click_script has no TTY to
  # answer a password/Touch ID prompt. No double-click lock needed: unlike the previous
  # `pmset noidle` background-process approach, this is one synchronous idempotent
  # command with no process to orphan -- a race at worst runs two toggles back to back.
  sleepPreventTogglePlugin = pkgs.writeShellScript "sleep_prevent_toggle.sh" ''
    STATE=$(pmset -g | awk '/SleepDisabled/ {print $2}')

    if [ "$STATE" = "1" ]; then
      sudo pmset -a disablesleep 0
    else
      sudo pmset -a disablesleep 1
    fi

    sketchybar --update
  '';

  # Tailscale-serve display plugin script: reflects whether Mediator
  # (127.0.0.1:43100) is currently proxied onto the tailnet.
  tailscaleServePlugin = pkgs.writeShellScript "tailscale_serve.sh" ''
    TAILSCALE="/run/current-system/sw/bin/tailscale"

    if "$TAILSCALE" serve status 2>/dev/null | grep -q '43100'; then
      sketchybar --set "$NAME" icon.color=${colors.green}
    else
      sketchybar --set "$NAME" icon.color=${colors.comment}
    fi
  '';

  # Tailscale-serve toggle click script: flips `tailscale serve` for Mediator
  # on/off. Requires the NOPASSWD sudoers grant in nix-darwin/config/security.nix
  # (scoped to exactly these two invocations), since this click_script has no
  # TTY to answer a password/Touch ID prompt.
  tailscaleServeTogglePlugin = pkgs.writeShellScript "tailscale_serve_toggle.sh" ''
    TAILSCALE="/run/current-system/sw/bin/tailscale"

    if "$TAILSCALE" serve status 2>/dev/null | grep -q '43100'; then
      sudo "$TAILSCALE" serve --https=443 off
    else
      sudo "$TAILSCALE" serve --bg 43100
    fi

    sketchybar --update
  '';

  sketchybarConfig = ''
    #!/bin/bash

    BACKGROUND="${colors.background}"
    BACKGROUND_TRANSPARENT="${colors.backgroundTransparent}"
    CURRENT_LINE="${colors.currentLine}"
    FOREGROUND="${colors.foreground}"
    COMMENT="${colors.comment}"
    CYAN="${colors.cyan}"
    GREEN="${colors.green}"
    ORANGE="${colors.orange}"
    PINK="${colors.pink}"
    PURPLE="${colors.purple}"
    RED="${colors.red}"
    YELLOW="${colors.yellow}"
    BLACK="${colors.black}"
    TRANSPARENT="${colors.transparent}"

    bar=(
      height=40
      color="$BACKGROUND_TRANSPARENT"
      shadow=on
      position=top
      sticky=on
      padding_left=8
      padding_right=8
      margin=8
      corner_radius=12
      blur_radius=30
      notch_width=200
      y_offset=4
                )
    sketchybar --bar "''${bar[@]}"

    default=(
      icon.font="Hack Nerd Font:Bold:14.0"
      icon.color="$FOREGROUND"
      icon.padding_left=6
      icon.padding_right=4
      label.font="Hack Nerd Font:Bold:12.0"
      label.color="$FOREGROUND"
      label.padding_left=4
      label.padding_right=6
      background.color="$TRANSPARENT"
      background.corner_radius=8
      background.height=28
      background.padding_left=2
      background.padding_right=2
        )
    sketchybar --default "''${default[@]}"

    sketchybar --add event aerospace_workspace_change

    sketchybar --add item apple_logo left \
                --set apple_logo \
                      icon="􀣺" \
                      icon.font="SF Pro:Bold:16.0" \
                      icon.color="$PURPLE" \
                      icon.padding_left=8 \
                      icon.padding_right=8 \
                      background.color="$TRANSPARENT" \
                      click_script="sketchybar --update"

    SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

    for i in "''${!SPACE_ICONS[@]}"; do
      sid="''${SPACE_ICONS[$i]}"
      # 10番目は表示を "0" にする
      if [ "$sid" = "10" ]; then
        display_icon="0"
      else
        display_icon="$sid"
      fi

      sketchybar --add item space.$sid left \
                  --set space.$sid \
                        icon="$display_icon" \
                        icon.font="Hack Nerd Font:Bold:12.0" \
                        icon.color="$COMMENT" \
                        icon.padding_left=8 \
                        icon.padding_right=8 \
                        background.color="$TRANSPARENT" \
                        background.corner_radius=6 \
                        background.height=24 \
                        click_script="aerospace workspace $sid" \
                        script="${aerospacePlugin}" \
                        update_freq=1 \
                  --subscribe space.$sid aerospace_workspace_change
    done

    sketchybar --add item front_app left \
                --set front_app \
                      icon.drawing=off \
                      label.font="Hack Nerd Font:Bold:12.0" \
                      label.color="$FOREGROUND" \
                      label.padding_left=12 \
                      script="${frontAppPlugin}" \
                --subscribe front_app front_app_switched

    sketchybar --add item time right \
                --set time \
                      icon="󰥔" \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$PURPLE" \
                      update_freq=1 \
                      script="${timePlugin}"

    sketchybar --add item date right \
                --set date \
                      icon="󰃭" \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$PINK" \
                      update_freq=1 \
                      script="${datePlugin}"

    sketchybar --add item separator_datetime right \
                --set separator_datetime \
                      icon=│ \
                      icon.color="$COMMENT" \
                      icon.padding_left=4 \
                      icon.padding_right=4 \
                      background.drawing=off

    sketchybar --add item battery right \
                --set battery \
                      update_freq=1 \
                      script="${batteryPlugin}"

    sketchybar --add item power_icon right \
                --set power_icon \
                      icon=󰁹 \
                      icon.font="Hack Nerd Font:Bold:16.0" \
                      icon.color="$GREEN" \
                      label.drawing=off

    # Sleep prevention toggle (sudo pmset -a disablesleep; update_freq=5 since it's a cheap
    # `pmset -g` read, not a heavier poll like the 1s-interval items below). Icon-only (no label,
    # matching power_icon) to keep the right-side item group narrow enough to clear the notch.
    sketchybar --add item sleep_prevent right \
                --set sleep_prevent \
                      icon=󰅶 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$COMMENT" \
                      label.drawing=off \
                      update_freq=5 \
                      script="${sleepPreventPlugin}" \
                      click_script="${sleepPreventTogglePlugin}"

    # Tailscale-serve toggle for Mediator (127.0.0.1:43100). Icon-only, matching
    # sleep_prevent, in the same power_bracket group.
    sketchybar --add item tailscale_serve right \
                --set tailscale_serve \
                      icon=󰛳 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$COMMENT" \
                      label.drawing=off \
                      update_freq=5 \
                      script="${tailscaleServePlugin}" \
                      click_script="${tailscaleServeTogglePlugin}"

    sketchybar --add item separator_power right \
                --set separator_power \
                      icon=│ \
                      icon.color="$COMMENT" \
                      icon.padding_left=4 \
                      icon.padding_right=4 \
                      background.drawing=off

    sketchybar --add item volume right \
                --set volume \
                      icon=󰕾 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$GREEN" \
                      update_freq=1 \
                      script="${volumePlugin}" \
                --subscribe volume volume_change

    sketchybar --add item wifi right \
                --set wifi \
                      icon=󰖩 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$CYAN" \
                      update_freq=1 \
                      script="${wifiPlugin}"

    sketchybar --add item separator_network right \
                --set separator_network \
                      icon=│ \
                      icon.color="$COMMENT" \
                      icon.padding_left=4 \
                      icon.padding_right=4 \
                      background.drawing=off

    sketchybar --add item memory right \
                --set memory \
                      icon=󰍛 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$YELLOW" \
                      update_freq=1 \
                      script="${memoryPlugin}"

    sketchybar --add item cpu right \
                --set cpu \
                      icon=󰻠 \
                      icon.font="Hack Nerd Font:Bold:14.0" \
                      icon.color="$ORANGE" \
                      update_freq=1 \
                      script="${cpuPlugin}"

    sketchybar --add bracket spaces_bracket '/space\..*/' \
                --set spaces_bracket \
                      background.color="$CURRENT_LINE" \
                      background.corner_radius=10 \
                      background.height=32

    sketchybar --add bracket system_bracket cpu memory \
                --set system_bracket \
                      background.color="$CURRENT_LINE" \
                      background.corner_radius=10 \
                      background.height=32

    sketchybar --add bracket network_bracket wifi volume \
                --set network_bracket \
                      background.color="$CURRENT_LINE" \
                      background.corner_radius=10 \
                      background.height=32

    sketchybar --add bracket power_bracket power_icon battery sleep_prevent tailscale_serve \
                --set power_bracket \
                      background.color="$CURRENT_LINE" \
                      background.corner_radius=10 \
                      background.height=32

    sketchybar --add bracket datetime_bracket date time \
                --set datetime_bracket \
                      background.color="$CURRENT_LINE" \
                      background.corner_radius=10 \
                      background.height=32

    AEROSPACE="/run/current-system/sw/bin/aerospace"
    FOCUSED=$("$AEROSPACE" list-workspaces --focused 2>/dev/null || echo "1")
    sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE="$FOCUSED"

    sketchybar --update
  '';
in
{
  programs.sketchybar.enable = true;
  programs.sketchybar.config.text = sketchybarConfig;
}
