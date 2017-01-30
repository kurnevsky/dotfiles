#!/bin/sh
xrandr | awk -v crtc=0 -v dir=left-of 'out { gsub("[^0-9.]", "", $2); print "--output " out " --mode " $1 " --rate " $2 " --crtc " crtc++ pout; pout = " --" dir " " out; out=0 } / connected/ { out=$1 }' | xargs xrandr --verbose
