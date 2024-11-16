#!/usr/bin/env sh

thermal_pressure=$(./plugins/macos-temp-tool -p | awk '{print $NF}')
LABEL="${thermal_pressure}"

sketchybar --set "$NAME" label="$LABEL"
