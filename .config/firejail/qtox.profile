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
private-etc fonts
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

mkdir ~/.config/tox
whitelist ~/.config/tox
mkdir ~/.cache/Tox
whitelist ~/.cache/Tox
