module Terminal (introMessage, runGame) where

import Control.Monad (unless, when)
import Data.Char (isAlphaNum, toUpper)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Game

introMessage :: IO ()
introMessage = putStr $ unlines
  [ "=== Welcome to Desert Explorer! ==="
  , ""
  , "Venture into a mysterious desert in search of lost treasures."
  , "But be careful: every step costs you precious water, and one wrong move"
  , "could land you in a pool of deadly lava..."
  , ""
  , "Controls:"
  , "  W - Move up"
  , "  S - Move down"
  , "  A - Move left"
  , "  D - Move right"
  , ""
  , "Map Symbols:"
  , "  @ - You (the player)"
  , "  T - Treasure"
  , "  W - Water source"
  , "  P - Portal"
  , "  L - Lava (deadly!)"
  , "  . - Desert"
  , ""
  , "Stay alert and plan your moves wisely."
  , "Good luck, explorer!"
  , ""
  ]

-- user input for a simple terminal-based game is just a single-line string
promptForInput :: IO Command
promptForInput = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  when eof $ putStrLn "" >> exitSuccess
  map toUpper . filter isAlphaNum <$> getLine

-- run a game in the terminal
runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame = either putStrLn loop . initialState
  where
    loop st = do
      print st
      unless (isFinalState st) $ do
        cmd <- promptForInput
        putStrLn ""
        case nextState st cmd of
          Nothing -> putStrLn "Invalid input, please try again" >> loop st
          Just (Left err) -> putStrLn err >> loop st
          Just (Right st') -> loop st'
