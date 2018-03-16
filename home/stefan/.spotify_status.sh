#!/bin/bash
artist=`playerctl metadata artist`
title=`playerctl metadata title`
p_status=`playerctl status`
delim="\xEF\xBD\x9C"
player_status="X"
if [ $p_status == "Playing" ]
then
player_status="\xE2\x96\xB6"
fi
if [ $p_status == "Paused" ]
then
player_status="="
fi
message=`echo "${player_status} ${artist} - ${title}" | cut -c1-45`
printf "${delim}${message}${delim}"
