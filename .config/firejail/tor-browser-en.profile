quiet

name tor-browser-en

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin tor-browser-en,env,bash,getconf,id,mkdir,rm,tar,xz
private-dev
private-etc fonts,gtk-2.0
private-tmp

include ~/.config/firejail/mem/4G.inc

mkdir ~/.tor-browser-en
whitelist ~/.tor-browser-en
