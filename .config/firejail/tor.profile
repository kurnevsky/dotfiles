quiet

name tor

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin tor
private-dev
private-etc tor
private-tmp

include ~/.config/firejail/mem/1G.inc

mkdir ~/.tor
whitelist ~/.tor
