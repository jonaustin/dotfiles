#!/bin/sh

DAY=`date +%a`
DAY=${DAY:0:2}
#OTHER=`date +"%e %b, %H:%M"`
#echo "$DAY $OTHER"
echo -n "<span color=\"black\">   `date +%H:%M`  </span>"
