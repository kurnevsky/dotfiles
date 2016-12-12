caps.drop all
seccomp
netfilter
tracelog
noroot
private-dev
shell none
net none
include /etc/firejail/whitelist-common.inc
whitelist /tmp/.X11-unix
whitelist ~/pdd
whitelist ~/.wine-pdd
