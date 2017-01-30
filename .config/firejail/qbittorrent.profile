name qbittorrent

caps.drop all
netfilter
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none
nogroups
nosound

private-dev
private-tmp
private-etc resolv.conf,fonts/
private-bin qbittorrent

mkdir ~/.local/share/data/qBittorrent
whitelist ~/.local/share/data/qBittorrent
mkdir ~/.config/qBittorrent
whitelist ~/.config/qBittorrent
mkdir ~/Torrents
whitelist ~/Torrents
