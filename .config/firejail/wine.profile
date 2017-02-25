quiet

name wine

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-dev
private-etc resolv.conf,fonts,gtk-3.0,pulse
private-tmp

mkdir ~/.wine
whitelist ~/.wine
