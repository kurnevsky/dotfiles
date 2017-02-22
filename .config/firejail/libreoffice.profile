name libreoffice

quiet

caps.drop all
net none
netfilter
nogroups
nonewprivs
noroot
protocol unix
seccomp
shell none

private-bin libreoffice,lobase,localc,lodraw,loffice,lofromtemplate,loimpress,lomath,loweb,lowriter,soffice,unopkg,sh,bash,dirname,basename,ls,sed,grep,uname
private-dev
private-etc fonts,gtk-2.0,gtk-3.0,libreoffice,passwd
private-tmp

blacklist ~/.ssh
