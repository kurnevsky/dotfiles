quiet

name mpv

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin mpv
private-dev
private-etc mpv,fonts,pulse
private-tmp

blacklist ~/.ssh
read-only ~
