#!/bin/sh
APPS=$(ls /etc/firejail ~/.config/firejail | egrep '\.profile$' | rev | cut -c 9- | rev | sort -u)
for APP in $APPS; do
    if [ -e "/bin/$APP" ]; then
        ln -s /bin/firejail /usr/local/bin/$APP
    fi
done
