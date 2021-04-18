local utils = require 'mp.utils'
local window_id;

function suspend()
   os.execute("xdg-screensaver suspend " .. window_id)
end

function resume()
   os.execute("xdg-screensaver resume " .. window_id)
end

function on_pause_change(name, value)
   if value then
      resume()
   else
      suspend()
   end
end

function find_window_id()
   local handle = io.popen("wmctrl -lx | awk '{ORS=\" \"; print $1; system(\"xcb-client-id \" $1)}' | grep " .. utils.getpid() .. " | cut -d\" \" -f1 | head -n1", 'r')
   local result = handle:read("*a")
   handle:close()
   if result ~= "" then
      window_id = result:gsub("%s+", " ")
      print("MPV window id is " .. window_id)
      if not mp.get_property_native("pause") then
         suspend()
      end
      mp.observe_property("pause", "bool", on_pause_change)
   else
      mp.add_timeout(1, find_window_id)
   end
end

mp.add_timeout(1, find_window_id)

function on_shutdown()
   if window_id and not mp.get_property_native("pause") then
      resume()
   end
end

mp.register_event("shutdown", on_shutdown)
