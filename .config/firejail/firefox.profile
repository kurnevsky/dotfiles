name firefox

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin firefox
private-dev
private-etc resolv.conf,fonts/,gtk-3.0/,pulse/
private-tmp

mkdir ~/.mozilla
whitelist ~/.mozilla
mkdir ~/.cache/mozilla/firefox
whitelist ~/.cache/mozilla/firefox
mkdir ~/Downloads
whitelist ~/Downloads
