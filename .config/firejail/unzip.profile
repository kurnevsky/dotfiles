quiet

name unzip

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin unzip
private-dev
private-etc _none_ # firejail requires non-empty list here so _none_ is a workaround.
private-tmp

blacklist ~/.ssh
