quiet

name skypeforlinux

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6,netlink
seccomp
shell none

private-bin skypeforlinux,sh,electron
# private-dev doesn't fit because of video0 device
private-etc resolv.conf,localtime,fonts,gtk-3.0,pulse
private-tmp

mkdir ~/.config/skypeforlinux
whitelist ~/.config/skypeforlinux
mkdir ~/Skype
whitelist ~/Skype
