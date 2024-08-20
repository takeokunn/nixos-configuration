#!/run/current-system/sw/bin/bash

FloatingVim=$(swaymsg -t get_tree | jq -c '.. | .floating_nodes? | arrays[] | select(.app_id=="FloatingVim")')

if [ -z $FloatingVim ]; then
  wezterm \
      --config initial_rows=10\
      --config initial_cols=70 \
      --config enable_tab_bar=false\
      --config window_background_opacity=0.4 \
      --config text_background_opacity=0.4 \
    start \
      --class FloatingVim nvim
else
  if [ "$(echo $FloatingVim | jq .focused)" = 'true' ]; then
    swaymsg "move window to scratchpad"
  else
    swaymsg "[app_id=\"FloatingVim\"] focus"
  fi
fi
