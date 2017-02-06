name tor-browser-en

quiet

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin tor-browser-en,env,bash,getconf,id
private-dev
private-etc fonts,gtk-2.0
private-tmp

mkdir ~/.tor-browser-en
whitelist ~/.tor-browser-en
