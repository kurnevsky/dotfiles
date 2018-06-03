quiet

name pidgin

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin pidgin,firefox
private-dev
private-etc resolv.conf,localtime,fonts,gtk-2.0,gtk-3.0,pulse,ssl,ca-certificates
private-tmp

whitelist ~/.gtkrc-2.0

mkdir ~/.purple
whitelist ~/.purple
