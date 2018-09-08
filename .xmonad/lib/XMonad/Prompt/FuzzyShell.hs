module XMonad.Prompt.FuzzyShell where

import Data.List
import Text.EditDistance
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell

-- | Defines the priority of completions that match only if the input contains a
-- typo. Higher values mean that such completions will be placed farther
-- compared to completions that that match without typos.
scale :: Int
scale = 5

-- | Number of typos the input can have.
typos :: Int
typos = 1

costs :: EditCosts
costs = EditCosts {
  deletionCosts = ConstantCost $ scale - typos,
  insertionCosts = ConstantCost 1,
  substitutionCosts = ConstantCost scale,
  transpositionCosts = ConstantCost scale
}

fuzzyCompl :: [String] -> String -> [String]
fuzzyCompl _ [] = []
fuzzyCompl l s =
  let weight = restrictedDamerauLevenshteinDistance costs s
      prefix = not . isPrefixOf s
  in map (\(_, _, e) -> e) $
       sort $
       filter (\(p, d, e) -> not p || d <= length e - length s + typos * scale) $
       map (\e -> (prefix e, weight e, e)) $
       filter (\e -> length e >= length s - typos) l

fuzzyShellPrompt :: XPConfig -> X ()
fuzzyShellPrompt c = do
  cmds <- io getCommands
  mkXPrompt Shell c (return . fuzzyCompl cmds) spawn
