#!/bin/sh

dbus-update-activation-environment --systemd DISPLAY XAUTHORITY GDK_PIXBUF_MODULE_FILE

systemctl --user start hm-graphical-session.target

xrdb ~/.Xresources

xsetroot -cursor_name left_ptr

syndaemon -d -t -k -R

numlockx

gnome-keyring-daemon

dunst &

volumeicon &

clipit &

STATUSBAR=${STATUSBAR:-taffybar}

if [ "$STATUSBAR" = "xmobar" ]
then
  nm-applet &

  trayer --edge top \
         --align left \
         --SetDockType true \
         --SetPartialStrut false \
         --expand true \
         --height 25 \
         --transparent true \
         --alpha 0 \
         --tint 0x000000 \
         --widthtype request \
         --monitor primary &
elif [ "$STATUSBAR" = "taffybar" ]
then
  nm-applet --indicator &
fi

picom -b --config ~/.config/picom.conf --dbus --glx-fshader-win "$(cat ~/.glsl/negative.glsl)"

export _JAVA_AWT_WM_NONREPARENTING=1

export QT_QPA_PLATFORMTHEME=qt5ct

xmonad 2>> ~/.xsession-errors

systemctl --user stop graphical-session.target
systemctl --user stop graphical-session-pre.target
