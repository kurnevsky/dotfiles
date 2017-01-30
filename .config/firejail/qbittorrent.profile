caps.drop all
netfilter
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none
nosound

private-dev
private-tmp
private-etc resolv.conf,fonts/
private-bin qbittorrent

whitelist /tmp/.X11-unix

mkdir ~/.local/share/data/qBittorrent
whitelist ~/.local/share/data/qBittorrent
mkdir ~/.config/qBittorrent
whitelist ~/.config/qBittorrent
mkdir ~/Torrents
whitelist ~/Torrents
