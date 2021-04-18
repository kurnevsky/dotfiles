local utils = require 'mp.utils'

local cmd = "sleep 1; wmctrl -lx | awk '{ORS=\" \"; print $1; system(\"xcb-client-id \" $1)}' | grep " .. utils.getpid() .. " | cut -d\" \" -f1 | head -n1 | xargs -r xdg-screensaver "

function on_pause_change(name, value)
   if value then
      os.execute(cmd .. "resume")
   else
      os.execute(cmd .. "suspend")
   end
end

mp.observe_property("pause", "bool", on_pause_change)
