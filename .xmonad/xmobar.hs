Config { font = "xft:DejaVu Sans:size=11"
       , bgColor = "black"
       , fgColor = "gray"
       , position = TopSize C 100 25
       , commands = [ Run Date "<fc=#ffcc00>%a %Y.%m.%d %T</fc>" "date" 10
                    , Run WeatherX "UMMS" [ ("clear", "🌣")
                                          , ("sunny", "🌣")
                                          , ("mostly clear", "🌤")
                                          , ("mostly sunny", "🌤")
                                          , ("partly sunny", "⛅")
                                          , ("fair", "🌑")
                                          , ("cloudy","☁")
                                          , ("overcast","☁")
                                          , ("partly cloudy", "⛅")
                                          , ("mostly cloudy", "🌧")
                                          , ("considerable cloudiness", "⛈")
                                          ]
                                          [ "--template", "<station> <hour> <tempC>°C <skyConditionS> <rh>% <pressure>Hg"
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
                    , Run CoreTemp [ "--template", "<core0>|<core1>°C"
                                   , "--Low", "60"
                                   , "--High", "80"
                                   , "--low", "darkgreen"
                                   , "--normal", "darkorange"
                                   , "--high", "darkred"
                                   ] 10
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
                    , Run Mpris2 "DeaDBeeF" [ "--template", "<artist> <title>"
                                            , "--maxtwidth", "50"
                                            ] 10
                    , Run UnsafeStdinReader
                    , Run Com "/bin/bash" [ "-c", "~/.xmonad/trayer-pad-icon.sh"
                                          ] "trayer" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%trayer% : %UnsafeStdinReader%}{%mpris2% : %dynnetwork% : %diskio% : %cpu% : %memory% : %swap% : %coretemp% : %battery% : %UMMS% : <action=`xterm -e zsh -c 'cal -m -y; sleep infinity'`>%date%</action>"
       }
