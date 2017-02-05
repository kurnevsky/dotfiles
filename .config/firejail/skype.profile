name skype

quiet

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin skype,sh,bash
private-dev
private-etc resolv.conf,fonts,pulse
private-tmp

mkdir ~/.Skype
whitelist ~/.Skype
mkdir ~/Skype
whitelist ~/Skype
