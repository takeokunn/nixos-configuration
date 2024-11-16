#!/bin/bash

# Get Wi-Fi network information
WI_FI_INFO=$(networksetup -listallhardwareports | awk '/Wi-Fi/{getline; print $2}')

# Get the current Wi-Fi network
CURRENT_NETWORK=$(echo "$WI_FI_INFO" | xargs networksetup -getairportnetwork | sed "s/Current Wi-Fi Network: //")

# Check if the current network is associated with an airport network
if echo "$CURRENT_NETWORK" | grep -q "You are not associated with an AirPort network"; then
    LABEL="N/A"
    sketchybar --set net_logo background.color=0xff3C3E4F --set net label.color=0xff1e1d2e
else
    sketchybar --set net_logo background.color=0xffE0A3AD --set net label.color=0xffECEFF4
    # Truncate the current network name if longer than 10 characters
    if [ "$(echo "$CURRENT_NETWORK" | awk '{ print length($1) }')" -gt 10 ]; then
        label=$(echo "$CURRENT_NETWORK" | awk '{ print substr($0, 1, 7) }')
        label=$(echo "$label" | sed 's/ *$//')
        LABEL="$label"...
    else
        LABEL=$(echo "$CURRENT_NETWORK" | awk '{ printf "%s", $1 }')
        # If there is a second word, print the first two characters followed by ...
        if [ "$(echo "$CURRENT_NETWORK" | awk '{ print NF }')" -gt 1 ]; then
            SECOND_WORD=$(echo "$CURRENT_NETWORK" | awk '{ printf "%s", substr($2, 1, 2) }')
            LABEL="$LABEL $SECOND_WORD..."
        fi
    fi
fi

sketchybar --set "$NAME" label="$LABEL"
