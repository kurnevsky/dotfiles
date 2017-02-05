name pidgin

quiet

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin pidgin
private-dev
private-etc resolv.conf,fonts,gtk-2.0,gtk-3.0,pulse,ssl,ca-certificates
private-tmp

mkdir ~/.purple
whitelist ~/.purple
