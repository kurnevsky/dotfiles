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
                    , Run StdinReader
                    , Run Com "/bin/bash" [ "-c", "~/.xmonad/trayer-pad-icon.sh"
                                          ] "trayer" 5
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%trayer% : %StdinReader%}{%battery% : %UMMS% : %date%"
       }
