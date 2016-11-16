#!/bin/bash

dev=/sys/class/leds/smc::kbd_backlight/brightness

current=$(cat "$dev")
adj=10
min=0
max=100

if [[ $1 == "up" ]]; then
    new=$(((current + adj) > max ? max : (current + adj)))
    echo "$new" | sudo tee "$dev"
elif [[ $1 == "down" ]]; then
    new=$(((current - adj) < min ? min : (current - adj)))
    echo "$new" | sudo tee "$dev"
else
    echo $current
fi
