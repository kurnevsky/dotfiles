caps.drop all
seccomp
protocol unix,inet,inet6
netfilter
tracelog
noroot
private-dev
shell none
include /etc/firejail/whitelist-common.inc
whitelist /tmp/.X11-unix
whitelist ~/.local/share/eiskaltdc++
whitelist ~/.config/eiskaltdc++
whitelist ~/Downloads
whitelist ~/Music
