#!/bin/bash -x

xrandr --output eDP1 --auto \
       --output DP2 --auto \
       --output DP1 --auto

if xrandr -q | grep -q '^DP1[[:space:]]*connected'; then
    xrandr --output eDP1 --pos 0x200 \
           --output DP2 --pos 2880x0 --rotate left \
           --output DP1 --pos 4080x200
fi
