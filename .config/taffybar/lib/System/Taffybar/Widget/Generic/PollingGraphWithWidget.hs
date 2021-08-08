-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widget.Generic.PollingGraphWithWidget (
  -- * Types
  GraphHandle,
  GraphConfig(..),
  GraphDirection(..),
  GraphStyle(..),
  -- * Constructors and accessors
  pollingGraphNewWithWidget,
  defaultGraphConfig
  ) where

import           Control.Concurrent
import qualified Control.Exception.Enclosed as E
import           Control.Monad
import           Control.Monad.IO.Class
import           GI.Gtk
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.Graph

pollingGraphNewWithWidget
  :: MonadIO m
  => GraphConfig -> Double -> (GI.Gtk.Widget -> IO [Double]) -> m GI.Gtk.Widget
pollingGraphNewWithWidget cfg pollSeconds action = liftIO $ do
  (graphWidget, graphHandle) <- graphNew cfg

  _ <- onWidgetRealize graphWidget $ do
       sampleThread <- foreverWithDelay pollSeconds $ do
         esample <- E.tryAny $ action graphWidget
         case esample of
           Left _ -> return ()
           Right sample -> graphAddSample graphHandle sample
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread

  return graphWidget
