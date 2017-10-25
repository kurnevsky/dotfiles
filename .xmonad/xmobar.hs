Config { font = "xft:DejaVu Sans:pixelsize=14:antialias=true:autohint=false"
       , bgColor = "black"
       , fgColor = "gray"
       , position = TopSize C 100 25
       , commands = [ Run Date "<fc=#ffcc00>%a %Y.%m.%d %T</fc>" "date" 10
                    , Run Weather "UMMS" [ "--template", "<station> <hour> <tempC>C <rh>% <pressure>Hg"
                                         ] 1000
                    , Run Battery [ "--template", "<acstatus>"
                                  , "--Low", "10"
                                  , "--High", "80"
                                  , "--low", "darkred"
                                  , "--normal", "darkorange"
                                  , "--high", "darkgreen"
                                  , "--"
                                  , "-O", "<left>% <fc=darkgreen>↻</fc>"
                                  , "-i", "<fc=darkgreen>∞</fc>"
                                  , "-o", "<left>% (<timeleft>)"
                                  ] 1000
                    , Run Swap [ "--template", "swap <used>M"
                               , "--Low", "100"
                               , "--High", "500"
                               , "--low", "darkgreen"
                               , "--normal", "darkorange"
                               , "--high", "darkred"
                               ] 10
                    , Run Memory [ "--template", "mem <usedratio>%"
                                 , "--Low", "50"
                                 , "--High", "90"
                                 , "--low", "darkgreen"
                                 , "--normal", "darkorange"
                                 , "--high", "darkred"
                                 ] 10
                    , Run Cpu [ "--template", "cpu <total>%"
                              , "--Low", "10"
                              , "--High", "90"
                              , "--low", "darkgreen"
                              , "--normal", "darkorange"
                              , "--high", "darkred"
                              ] 10
                    , Run DiskIO [("sda", "io ▼ <write> ▲ <read>")] [] 10
                    , Run DynNetwork [ "--template", "<dev> ▼ <rx>K ▲ <tx>K"
                                     , "--Low", "131072"
                                     , "--High", "1048576"
                                     , "--low", "darkgreen"
                                     , "--normal", "darkorange"
                                     , "--high", "darkred"
                                     ] 10
                    -- , Run Mpris2 "DeaDBeeF" [ "--template", "<artist> <title>"
                    --                         , "--maxtwidth", "50"
                    --                         ] 10
                    , Run UnsafeStdinReader
                    , Run Com "/bin/bash" [ "-c", "~/.xmonad/trayer-pad-icon.sh"
                                          ] "trayer" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       -- , template = "%trayer% : %UnsafeStdinReader%}{%mpris2% : %dynnetwork% : %diskio% : %cpu% : %memory% : %swap% : %battery% : %UMMS% : <action=`alacritty -e zsh -c 'cal -m -y; sleep infinity'`>%date%</action>"
       , template = "%trayer% : %UnsafeStdinReader%}{%dynnetwork% : %diskio% : %cpu% : %memory% : %swap% : %battery% : %UMMS% : <action=`alacritty -e zsh -c 'cal -m -y; sleep infinity'`>%date%</action>"
       }
