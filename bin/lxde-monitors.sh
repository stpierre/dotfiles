#!/bin/bash -x

xrandr --output eDP1 --auto \
       --output DP1-1 --auto \
       --output DP1-2 --auto

if xrandr -q | grep -q '^DP1-1[[:space:]]*connected'; then
    xrandr --output eDP1 --pos 0x600 \
           --output DP1-1 --pos 1920x0 --rotate left \
           --output DP1-2 --pos 3120x400
fi
