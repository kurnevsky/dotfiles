quiet

name bitcoind

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin bitcoind
private-dev
private-etc _none_ # firejail requires non-empty list here so _none_ is a workaround.
private-tmp

include ~/.config/firejail/mem/4G.inc

mkdir ~/.bitcoin
whitelist ~/.bitcoin
