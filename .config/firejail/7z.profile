quiet

name 7z

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin 7z,7za,7zr,sh,bash
private-dev
private-etc _none_ # firejail requires non-empty list here so _none_ is a workaround.
private-tmp

blacklist ~/.ssh
