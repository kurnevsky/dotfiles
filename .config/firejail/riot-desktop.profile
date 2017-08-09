quiet

name riot

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6,netlink
seccomp
shell none

private-bin riot-desktop,sh,bash,electron
private-etc resolv.conf,localtime,fonts,gtk-3.0,pulse
private-tmp

# private-dev doesn't fit because of video0 device
whitelist /dev/dri
whitelist /dev/null
whitelist /dev/full
whitelist /dev/zero
whitelist /dev/random
whitelist /dev/snd
whitelist /dev/urandom
whitelist /dev/shm
whitelist /dev/video0

mkdir ~/.config/Riot
whitelist ~/.config/Riot
mkdir ~/Riot
whitelist ~/Riot
