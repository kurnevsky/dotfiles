quiet

name monerod

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin monerod
private-dev
private-etc _none_ # firejail requires non-empty list here so _none_ is a workaround.
private-tmp

mkdir ~/.bitmonero
whitelist ~/.bitmonero
