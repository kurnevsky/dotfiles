#!/bin/sh

systemctl --user start hm-graphical-session.target

xrdb ~/.Xresources

export _JAVA_AWT_WM_NONREPARENTING=1

exec ~/.xsession
