caps.drop all
seccomp
protocol unix,inet,inet6
netfilter
noroot
private-dev
private-tmp
private-etc mime.types,fonts/,xdg/,gtk-3.0/,X11/,pulse/
shell none
include /etc/firejail/whitelist-common.inc
whitelist /tmp/.X11-unix
whitelist ~/.tor-browser-en
