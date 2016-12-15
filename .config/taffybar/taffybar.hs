import System.Taffybar

import Data.Maybe
import Control.Applicative
import System.Directory
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
import Graphics.UI.Gtk

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

networksList :: IO [String]
networksList = filter (maybe False (/= '.') . listToMaybe) <$> getDirectoryContents "/sys/class/net"

isShowNetwork :: String -> Bool
isShowNetwork ('w' : 'l' : 'p' : _) = True
isShowNetwork ('e' : 'n' : 'p' : _) = True
isShowNetwork _ = False

net :: IO [IO Widget]
net = do
  networks <- networksList
  return $
    map (\network -> netMonitorNewWith 1 network 1 (network ++ " ▼ $inKB$ ▲ $outKB$")) $
    filter isShowNetwork networks

disk = dioMonitorNew diskCfg 1 "sda"

cpu = pollingGraphNew cpuCfg 1 cpuCallback

mem = pollingGraphNew memCfg 1 memCallback

battery = batteryBarNew defaultBatteryConfig 10

tray = systrayNew

weather = weatherNew (defaultWeatherConfig "UMMS") { weatherTemplate = "$stationPlace$ $hour$ $tempC$C $humidity$% $pressure$Hg" } 10

clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M:%S</span>" 1

main = do
  n <- net
  defaultTaffybar defaultTaffybarConfig
    { startWidgets = [pager]
    , endWidgets = [clock, weather, tray, battery, mem, cpu, disk] ++ n ++ [mpris, mpris2]
    }
