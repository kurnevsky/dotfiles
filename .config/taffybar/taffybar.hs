import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.MPRIS2
import System.Taffybar.Battery
import System.Taffybar.CPUMonitor
import System.Taffybar.DiskIOMonitor
import System.Taffybar.NetMonitor
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Information.Memory
import System.Information.CPU

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

memCfg = defaultGraphConfig
  { graphDataColors = [(1, 0, 0, 1)]
  , graphLabel = Just "m"
  , graphDirection = RIGHT_TO_LEFT
  }

cpuCfg = defaultGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "c"
  , graphDirection = RIGHT_TO_LEFT
  }

diskCfg = defaultGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "d"
  , graphDirection = RIGHT_TO_LEFT
  }

pager = taffyPagerNew defaultPagerConfig

mpris2 = mpris2New

mpris = mprisNew defaultMPRISConfig

wifi = netMonitorNewWith 1 "wlp2s0" 1 "lan ▼ $inKB$ ▲ $outKB$ kb/s"

disk = dioMonitorNew diskCfg 1 "sda"

cpu = pollingGraphNew cpuCfg 1 cpuCallback

mem = pollingGraphNew memCfg 1 memCallback

battery = batteryBarNew defaultBatteryConfig 10

tray = systrayNew

weather = weatherNew (defaultWeatherConfig "UMMS") { weatherTemplate = "$stationPlace$ $hour$ $tempC$C $humidity$% $pressure$Hg" } 10

clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M:%S</span>" 1

main = defaultTaffybar defaultTaffybarConfig
  { startWidgets = [pager]
  , endWidgets = [clock, weather, tray, battery, mem, cpu, disk, wifi, mpris, mpris2]
  }
