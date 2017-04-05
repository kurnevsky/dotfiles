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

private-bin qtox
# private-dev doesn't fit because of video0 device
private-etc fonts
private-tmp

mkdir ~/.config/tox
whitelist ~/.config/tox
