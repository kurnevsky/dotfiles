{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Exception
import Control.Monad.Trans.Reader
import Data.Text (pack)
import qualified GI.Gtk
import GI.Gtk.Objects.Widget (widgetSetTooltipMarkup)
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context (TaffybarConfig)
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Util ((<|||>), postGUIASync)
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingGraphWithWidget
import System.Taffybar.Widget.Util (widgetSetClassGI)

myGraphConfig, netCfg, memCfg, cpuCfg, diskCfg :: GraphConfig
myGraphConfig = defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphBackgroundColor = (0, 0, 0, 0)
  , graphDirection = RIGHT_TO_LEFT
  }

netCfg = myGraphConfig
  { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
  , graphLabel = Just "net "
  }

memCallback :: GI.Gtk.Widget -> IO [Double]
memCallback widget = do
  mi <- parseMeminfo
  let tooltip = showMemoryInfo "Memory: $used$" 3 mi
  postGUIASync $ widgetSetTooltipMarkup widget $ Just tooltip
  return [memoryUsedRatio mi, memorySwapUsedRatio mi]

cpuCallback :: GI.Gtk.Widget -> IO [Double]
cpuCallback widget = do
  (_, systemLoad, totalLoad) <- cpuLoad
  let totalLoadStr = show $ round $ totalLoad * 100
      systemLoadStr = show $ round $ systemLoad * 100
      tooltip = pack $ "Total:\t" ++ totalLoadStr ++ "%\nSystem:\t" ++ systemLoadStr ++ "%"
  postGUIASync $ widgetSetTooltipMarkup widget $ Just tooltip
  return [totalLoad, systemLoad]

memCfg = myGraphConfig
  { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
  , graphLabel = Just "mem "
  }

cpuCfg = myGraphConfig
  { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
  , graphLabel = Just "cpu "
  }

diskCfg = myGraphConfig
  { graphDataColors = [(1, 1, 1, 1), (0.5, 0.5, 0.5, 1)]
  , graphLabel = Just "io "
  }

mpris2 = mpris2New

withClass c w = widgetSetClassGI w "graph" >> return w

net = networkGraphNewWith defaultNetworkGraphConfig { networkGraphGraphConfig = netCfg
                                                    , networkGraphScale = (/ (8 * 1024 * 1024))
                                                    } >>= withClass "graph"

disk = dioMonitorNew diskCfg 1 "sda" >>= withClass "graph"

cpu = pollingGraphNewWithWidget cpuCfg 1 cpuCallback >>= withClass "graph"

mem = pollingGraphNewWithWidget memCfg 1 memCallback >>= withClass "graph"

battery = batteryIconNew

tray = sniTrayNew

weather = weatherNew (defaultWeatherConfig "UMMS") { weatherTemplate = "$hour$ $tempC$°C" } 10

clock = textClockNewWith defaultClockConfig
  { clockFormatString = "<span fgcolor='gold'>%a %Y.%m.%d %T</span>"
  , clockUpdateStrategy = RoundedTargetInterval 1 0
  }

handleException :: WindowIconPixbufGetter -> WindowIconPixbufGetter
handleException getter = \size windowData ->
                           ReaderT $ \c ->
                                       catch (runReaderT (getter size windowData) c) $ \(_ :: SomeException)
                                                                                       -> return Nothing

myGetWindowIconPixbuf :: WindowIconPixbufGetter
myGetWindowIconPixbuf = scaledWindowIconPixbufGetter $
  handleException getWindowIconPixbufFromDesktopEntry <|||>
  handleException getWindowIconPixbufFromClass <|||>
  handleException getWindowIconPixbufFromEWMH

myWorkspacesConfig = defaultWorkspacesConfig
  { showWorkspaceFn = hideEmpty
  , getWindowIconPixbuf = myGetWindowIconPixbuf
  , iconSort = return . id
  }

workspaces = workspacesNew myWorkspacesConfig

layout = layoutNew defaultLayoutConfig

windowsW = windowsNew defaultWindowsConfig

taffybarConfig :: TaffybarConfig
taffybarConfig =
  let myConfig = defaultSimpleTaffyConfig
        { startWidgets = workspaces : map (>>= buildContentsBox) [ layout, windowsW ]
        , endWidgets = map (>>= buildContentsBox)
          [ clock
          , weather
          , tray
          , battery
          , mem
          , cpu
          , disk
          , net
          , mpris2
          ]
        , barHeight = 25
        }
  in withBatteryRefresh $
     withLogServer $
     withToggleServer $
     toTaffyConfig myConfig

main :: IO ()
main = do
  taffyLogger <- getLogger "System.Taffybar"
  trayLogger <- getLogger "StatusNotifier.Tray"
  saveGlobalLogger $ setLevel WARNING taffyLogger
  saveGlobalLogger $ setLevel WARNING trayLogger
  startTaffybar taffybarConfig
