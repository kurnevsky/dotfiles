name qbittorrent

caps.drop all
netfilter
nogroups
nonewprivs
noroot
nosound
protocol unix,inet,inet6
seccomp
shell none

private-bin qbittorrent
private-dev
private-etc resolv.conf,fonts/
private-tmp

mkdir ~/.local/share/data/qBittorrent
whitelist ~/.local/share/data/qBittorrent
mkdir ~/.config/qBittorrent
whitelist ~/.config/qBittorrent
mkdir ~/Torrents
whitelist ~/Torrents
