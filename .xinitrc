#!/bin/sh

systemctl --user start hm-graphical-session.target

xrdb ~/.Xresources

picom -b --config ~/.config/picom.conf --dbus --glx-fshader-win "$(cat ~/.glsl/negative.glsl)"

export _JAVA_AWT_WM_NONREPARENTING=1

exec ~/.xsession
