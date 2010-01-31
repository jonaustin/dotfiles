#!/bin/sh
title=`mpc -p 6602 -f "%artist% - %title% - %album%"| head -1 | sed 's/^volume: [0-9]*%.*/(nothing)/'`
position=`mpc -p 6602 | head -2 | tail -1 | sed 's/.*\(([0-9]*%)\)/\1/'`
echo "Playing: $title $position "
