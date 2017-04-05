quiet

name torchat

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin torchat,sh,chmod,cat,python2,tor
private-dev
private-etc fonts,gtk-2.0
private-tmp

mkdir ~/.torchat
whitelist ~/.torchat
