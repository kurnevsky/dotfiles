quiet

name zathura

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin zathura
private-dev
private-etc fonts
private-tmp

include ~/.config/firejail/mem/1G.inc

blacklist ~/.ssh
read-only ~
mkdir ~/.local/share/zathura
read-write ~/.local/share/zathura
mkdir ~/Print
read-write ~/Print
