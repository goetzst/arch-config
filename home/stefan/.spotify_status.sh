#!/bin/bash
artist=`playerctl metadata artist`
title=`playerctl metadata title`
p_status=`playerctl status`
player_status="X"
if [ $p_status == "Playing" ]
then
player_status="\xE2\x96\xB6"
fi
if [ $p_status == "Paused" ]
then
player_status="="
fi
printf "${player_status} ${artist} - ${title}"
