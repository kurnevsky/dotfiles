quiet

name qtox

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin toxic
private-dev
private-etc localtime
private-tmp

include ~/.config/firejail/mem/1G.inc

mkdir ~/.config/tox
whitelist ~/.config/tox
