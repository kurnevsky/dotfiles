{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.FullscreenNoBorders where

import Data.List
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import qualified XMonad.Util.ExtensibleState as XS

newtype FullscreenNoBordersState a = FullscreenNoBordersState [a]
  deriving Typeable

instance ExtensionClass (FullscreenNoBordersState Window) where
  initialValue = FullscreenNoBordersState []

data FullscreenNoBordersAmbiguity = FullscreenNoBordersAmbiguity
  deriving (Read, Show)

instance SetsAmbiguous FullscreenNoBordersAmbiguity where
  hiddens FullscreenNoBordersAmbiguity wset mst wrs = do
    FullscreenNoBordersState ws <- XS.get
    return ws

data FullscreenNoBorders a = FullscreenNoBorders
  deriving (Read, Show)

instance LayoutModifier FullscreenNoBorders Window where
  handleMess FullscreenNoBorders m = do
    case fromMessage m of
      Just (AddFullscreen win) ->
        XS.modify $ \(FullscreenNoBordersState ws) -> FullscreenNoBordersState (win : ws)
      Just (RemoveFullscreen win) ->
        XS.modify $ \(FullscreenNoBordersState ws) -> FullscreenNoBordersState (delete win ws)
      _ ->
        return ()
    return Nothing
