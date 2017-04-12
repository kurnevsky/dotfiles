quiet

name bitcoin-qt

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin bitcoin-qt
private-dev
private-etc fonts
private-tmp

mkdir ~/.bitcoin
whitelist ~/.bitcoin
