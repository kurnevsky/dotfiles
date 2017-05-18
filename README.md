List of used software
====

xmonad - windows manager
===

Consists of 3 package:
* xmonad
* xmonad-contrib
* xmonad-extras

Install them from git [repositories](https://github.com/xmonad) with command `cabal install --enable-optimisation=2`.

Before the installation apply patches:
For xmonad: `git apply ~/.xmonad/patches/optimise.patch`
For xmonad-contrib: `git apply ~/.xmonad/patches/fullscrean.patch`

xmobar - top bar
===

Install it from git [repository](https://github.com/jaor/xmobar) with command `cabal install --enable-optimisation=2 --flags=all_extensions`.

trayer-srg - system tray
===

Install it from aur with command `pacaur -S trayer-srg`.

compton - composite manager
===

Install it with command `pacaur -S compton`.

dunst - notifier
===

Install it with command `pacaur -S dunst`.

parcellite - clipboard manager
===

Install it with command `pacaur -S parcellite`.

volumeicon - volume tray icon
===

Install it with command `pacaur -S volumeicon`.

xscreensaver - screensaver
===

Install it from aur with command `pacaur -S xscreensaver-arch-logo`.

xbacklight - backlight control
===

Install it with command `pacaur -S xorg-xbacklight`.

xcalib - invert colors
===

Install it from aur with command `pacaur -S xcalib`.

maim - take screenshots
===

Install it with command `pacaur -S maim`.
