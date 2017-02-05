#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xrdb ~/.Xresources

xsetroot -cursor_name left_ptr

syndaemon -d -t -k -R

numlockx

gnome-keyring-daemon

dunst &

volumeicon &

parcellite &

nm-applet &

trayer --edge top --align left --SetDockType true --SetPartialStrut false --expand true --height 25 --transparent true --alpha 0 --tint 0x000000 --widthtype request --monitor 0 &

compton -b -f -I 0.10 -O 0.10 --backend glx --vsync opengl --dbus

exec xmonad 2>> ~/.xsession-errors
