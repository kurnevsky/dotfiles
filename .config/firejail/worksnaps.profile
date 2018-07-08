quiet

name worksnaps

caps.drop all
netfilter
nogroups
nonewprivs
noroot
protocol unix,inet,inet6
seccomp
shell none

private-bin worksnaps,java,env,sh,bash,realpath,dirname,uname,chmod,which,sed
private-dev
private-etc resolv.conf,localtime,passwd,nsswitch.conf,ssl,fonts,java-8-openjdk,java9-openjdk
private-tmp

include ~/.config/firejail/mem/1G.inc

mkdir ~/timetracker
whitelist ~/timetracker
