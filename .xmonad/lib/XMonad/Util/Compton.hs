{-# LANGUAGE OverloadedStrings #-}

module XMonad.Util.Compton where

import Data.Maybe
import Data.Word
import DBus
import DBus.Client
import XMonad

dpyName :: Display -> String
dpyName dpy = map replace $ displayString dpy where
  replace ':' = '_'
  replace '.' = '_'
  replace c = c

inversionStatus :: Display -> Window -> X Bool
inversionStatus dpy w =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_get")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [ toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                ]
             }
  in io $ do client <- connectSession
             status <- call_ client mc
             disconnect client
             return $ (== 1) $ fromJust $ (fromVariant :: Variant -> Maybe Word16) $ head $ methodReturnBody status

invert :: Display -> Window -> Bool -> X ()
invert dpy w status =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_set")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [ toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                , toVariant ((if status then 1 else 0) :: Word16)
                                ]
             }
  in io $ do client <- connectSession
             callNoReply client mc
             disconnect client
