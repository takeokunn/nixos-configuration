#!/bin/sh

# The volume_change event supplies a $INFO variable in which the current volume
# percentage is passed to the script.

if [ "$SENDER" = "volume_change" ]; then
  VOLUME="$INFO"

  case "$VOLUME" in
    [6-9][0-9]|100) ICON="<U+F057E>"
    ;;
    [3-5][0-9]) ICON="<U+F0580>"
    ;;
    [1-9]|[1-2][0-9]) ICON="<U+F057F>"
    ;;
    *) ICON="<U+F0581>"
  esac

  sketchybar --set "$NAME" icon="$ICON" label="$VOLUME%"
fi
