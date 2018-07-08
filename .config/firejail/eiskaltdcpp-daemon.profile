quiet

name eiskaltdcpp

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin eiskaltdcpp-daemon,eiskaltdcpp-qt,eiskaltdcpp-gtk
private-dev
private-etc resolv.conf,localtime,fonts,gtk-2.0,pulse
private-tmp

include ~/.config/firejail/mem/1G.inc

mkdir ~/.local/share/eiskaltdc++
whitelist ~/.local/share/eiskaltdc++
mkdir ~/.config/eiskaltdc++
whitelist ~/.config/eiskaltdc++
mkdir ~/Downloads
whitelist ~/Downloads
mkdir ~/Music
whitelist ~/Music
