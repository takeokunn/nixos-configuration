#!/bin/sh

PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
CHARGING="$(pmset -g batt | grep 'AC Power')"

if [ "$PERCENTAGE" = "" ]; then
  exit 0
fi

case "${PERCENTAGE}" in
  9[0-9]|100) ICON="<U+F240>"
  ;;
  [6-8][0-9]) ICON="<U+F241>"
  ;;
  [3-5][0-9]) ICON="<U+F242>"
  ;;
  [1-2][0-9]) ICON="<U+F243>"
  ;;
  *) ICON="<U+F244>"
esac

if [[ "$CHARGING" != "" ]]; then
  ICON="<U+F0E7>"
fi

# The item invoking this script (name $NAME) will get its icon and label
# updated with the current battery status
sketchybar --set "$NAME" icon="$ICON" label="${PERCENTAGE}%"
