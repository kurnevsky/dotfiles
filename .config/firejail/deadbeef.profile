quiet

name deadbeef

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin deadbeef
private-dev
private-etc fonts,gtk-2.0,gtk-3.0,pulse
private-tmp

include ~/.config/firejail/mem/1G.inc

blacklist ~/.ssh
read-only ~
mkdir ~/.config/deadbeef
read-write ~/.config/deadbeef
mkdir ~/.cache/deadbeef
read-write ~/.cache/deadbeef
