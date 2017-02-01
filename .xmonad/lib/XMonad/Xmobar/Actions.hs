module XMonad.Xmobar.Actions (stripActions) where

import Text.Regex (Regex, subRegex, mkRegex, matchRegex)

stripActions :: String -> String
stripActions s = case matchRegex actionRegex s of
  Nothing -> s
  Just _  -> stripActions strippedOneLevel
  where
      strippedOneLevel = subRegex actionRegex s "[action=\\1\\2]\\3[/action]"

actionRegex :: Regex
actionRegex = mkRegex "<action=`?([^>`]*)`?( +button=[12345]+)?>(.+)</action>"
