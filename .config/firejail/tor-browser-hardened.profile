quiet

name tor-browser-hardened

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin tor-browser-hardened,env,bash,getconf,id,mkdir,rm,tar,xz
private-dev
private-etc fonts,gtk-2.0
private-tmp

mkdir ~/.tor-browser-hardened
whitelist ~/.tor-browser-hardened
