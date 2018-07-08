quiet

name unrar

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin unrar
private-dev
private-etc _none_ # firejail requires non-empty list here so _none_ is a workaround.
private-tmp

include ~/.config/firejail/mem/4G.inc

blacklist ~/.ssh
