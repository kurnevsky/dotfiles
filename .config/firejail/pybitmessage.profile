quiet

name pybitmessage

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin pybitmessage,sh,bash,python2,python2.7
private-dev
private-etc fonts
private-tmp

mkdir ~/.config/PyBitmessage
whitelist ~/.config/PyBitmessage
