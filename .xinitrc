#!/bin/sh

dbus-update-activation-environment --systemd DISPLAY XAUTHORITY

xrdb ~/.Xresources

xsetroot -cursor_name left_ptr

syndaemon -d -t -k -R

numlockx

gnome-keyring-daemon

dunst &

volumeicon &

parcellite &

nm-applet &

trayer --edge top --align left --SetDockType true --SetPartialStrut false --expand true --height 25 --transparent true --alpha 0 --tint 0x000000 --widthtype request --monitor primary &

picom -b --config ~/.config/picom.conf --dbus --glx-fshader-win "$(cat ~/.glsl/negative.glsl)"

export _JAVA_AWT_WM_NONREPARENTING=1

export QT_QPA_PLATFORMTHEME=qt5ct

exec xmonad 2>> ~/.xsession-errors
