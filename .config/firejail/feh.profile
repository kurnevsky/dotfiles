quiet

name feh

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin feh,convert,jpegtran
private-dev
private-etc fonts
private-tmp

include ~/.config/firejail/mem/1G.inc

blacklist ~/.ssh
read-only ~
read-write ~/.fehbg
