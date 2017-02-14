name tkabber

quiet

caps.drop all
netfilter
nogroups
nonewprivs
noroot
nosound
protocol unix,inet,inet6
seccomp
shell none

private-bin tkabber,sh,bash,wish,firefox
private-dev
private-etc resolv.conf,localtime,fonts,ssl,ca-certificates
private-tmp

mkdir ~/.tkabber
whitelist ~/.tkabber
