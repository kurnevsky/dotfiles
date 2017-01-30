name firefox

caps.drop all
netfilter
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none
nogroups

private-dev
private-tmp
private-etc resolv.conf,fonts/,gtk-3.0/,pulse/
private-bin firefox

mkdir ~/.mozilla
whitelist ~/.mozilla
mkdir ~/.cache/mozilla/firefox
whitelist ~/.cache/mozilla/firefox
mkdir ~/Downloads
whitelist ~/Downloads
