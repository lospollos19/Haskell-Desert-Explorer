module Main where

import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)
import Game
import GUI (runGUI)
import Terminal (introMessage, runGame)


defaultConfig :: DesertExplorerConfig
defaultConfig = DesertExplorerConfig
  { sight = 5
  , maxWaterCapacity = 100
  , seedG = 44
  , treasureChance = 10
  , waterChance = 10
  , portalChance = 5
  , singleLavaChance = 5
  , surroundedLavaChance = 20
  }

parseConfig :: [String] -> Maybe DesertExplorerConfig
parseConfig [] = Just defaultConfig
parseConfig args = case mapM readMaybe args of
  Just [s, maxWater, seed, t, w, p, l, ll] -> Just DesertExplorerConfig
    { sight = s
    , maxWaterCapacity = maxWater
    , seedG = seed
    , treasureChance = t
    , waterChance = w
    , portalChance = p
    , singleLavaChance = l
    , surroundedLavaChance = ll
    }
  _ -> Nothing

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " [--terminal] [<sight> <maxWater> <seed> <treasure%> <water%> <portal%> <lava%> <surroundedLava%>]"

main :: IO ()
main = do
  args <- getArgs
  let (terminal, rest) = case args of
        ("--terminal" : xs) -> (True, xs)
        xs -> (False, xs)
  case parseConfig rest of
    Nothing -> usage
    Just config
      | terminal -> introMessage >> runGame config
      | otherwise -> runGUI config
