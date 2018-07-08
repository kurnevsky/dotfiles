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
private-etc resolv.conf,fonts
private-tmp

include ~/.config/firejail/mem/1G.inc

mkdir ~/.config/PyBitmessage
whitelist ~/.config/PyBitmessage
