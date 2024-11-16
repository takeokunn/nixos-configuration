#!/usr/bin/env osascript

tell application "Music"
    if favorited of current track is true then
        set favorited of current track to false
        do shell script "sketchybar -m --set music icon=♥ icon.color=0xbaffffff"
      else
        set favorited of current track to true
        do shell script "sketchybar -m --set music icon="
    end if
end tell

delay 1

do shell script "bash ~/.config/sketchybar/scripts/music.sh"
