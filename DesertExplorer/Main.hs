module Main where
import GUI
import Game
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Environment (getArgs)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, newTVarIO, readTVarIO)
import Graphics.Gloss



main :: IO ()
main = do
  let config = DesertExplorerConfig
                { sight = 5
                , maxWaterCapacity = 100
                , seedG = 44
                , treasureChance = 10
                , waterChance = 10
                , portalChance = 5
                , singleLavaChance = 5
                , surroundedLavaChance = 5
                }
  case initialState config of
    Left err -> putStrLn err
    Right world -> do
          gameVar <- newTVarIO world
          playIO
            (InWindow "Desert Explorer (Graphical)" (1000, 800) (10, 10))
            white
            10
            gameVar
            drawWorldSTM
            handleInputSTM
            updateWorldSTM


{-
maintemp :: IO ()
maintemp = do
  args <- getArgs
  case mapM readMaybe args of
    Just [sight, maxWater, seed, treasure, water, portal, lava, surroundedLava] -> do
      let config = DesertExplorerConfig
            { sight = sight
            , maxWaterCapacity = maxWater
            , seedG = seed
            , treasureChance = treasure
            , waterChance = water
            , portalChance = portal
            , singleLavaChance = lava
            , surroundedLavaChance = surroundedLava
            }
      case initialState config of
        Left err -> putStrLn err
        Right world -> do
          gameVar <- newTVarIO world
          play
            (InWindow "Desert Explorer (Graphical)" (1000, 800) (10, 10))
            white
            10
            gameVar
            drawWorldSTM
            handleInputSTM
            updateWorldSTM
    _ -> putStrLn "Usage: ./main <sight> <maxWater> <seed> <treasure%> <water%> <portal%> <lava%> <surroundedLava%>"
-}
