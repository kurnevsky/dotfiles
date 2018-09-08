module XMonad.Prompt.FuzzyShell where

import Data.List
import System.Directory
import Text.EditDistance
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run

editCosts :: EditCosts
editCosts = EditCosts {
  deletionCosts = ConstantCost 3,
  insertionCosts = ConstantCost 1,
  substitutionCosts = ConstantCost 2,
  transpositionCosts = ConstantCost 2
}

fuzzyCompl :: [String] -> String -> [String]
fuzzyCompl _ [] = []
fuzzyCompl l s =
  let weight = levenshteinDistance editCosts s
      prefix = not . isPrefixOf s
  in map (\(_, _, e) -> e) $
       sort $
       filter (\(p, d, e) -> not p || d <= length e) $
       map (\e -> (prefix e, weight e, e)) l

fzfCompl :: FilePath -> [String] -> String -> IO [String]
fzfCompl _ _ [] = return []
fzfCompl fzf l s =
  fmap lines $ runProcessWithInput fzf ["-f", s] $ unlines l

fuzzyShellPrompt :: XPConfig -> X ()
fuzzyShellPrompt c = do
  cmds <- io getCommands
  maybeFzf <- io $ findExecutable "fzf"
  let compl = case maybeFzf of
        Just fzf -> fzfCompl fzf cmds
        Nothing -> return . fuzzyCompl cmds
  mkXPrompt Shell c compl spawn
