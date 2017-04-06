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

mkdir ~/.tor
whitelist ~/.tor
