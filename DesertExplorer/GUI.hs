{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


module GUI where
import Graphics.Gloss
import Game
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isAlphaNum, toUpper)
import Control.Monad (unless)
import Text.Read (readMaybe)
import System.Random (mkStdGen, randomR)
import Control.Concurrent (getNumCapabilities)
import System.Environment (getArgs)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, newTVarIO, readTVarIO)


type GameVar = TVar DesertExplorerGameState


drawWorldSTM :: GameVar -> IO Picture
drawWorldSTM gameVar = do
  state <- readTVarIO gameVar
  return $ drawWorld state

updateWorldSTM :: Float -> GameVar -> IO GameVar
updateWorldSTM _ = return

handleInputSTM :: Event -> GameVar -> IO GameVar
handleInputSTM event gameVar = atomically $ do
  state <- readTVar gameVar
  let newState = handleInput event state
  writeTVar gameVar newState
  return gameVar


handleInput :: Event -> DesertExplorerGameState -> DesertExplorerGameState
handleInput (EventKey (Char c) Down _ _) state
        | c `elem` "wasdWASD" && not (isFinalState state) =
                        case nextState state [toUpper c] of
                                Just (Right newState) -> newState
                                _ -> state
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) _ = unsafePerformIO exitSuccess `seq` undefined
handleInput _ state = state


drawWorld :: DesertExplorerGameState -> Picture
drawWorld s@DesertExplorerGameState{..}
  | isLooseLavaState s = centerText ("You walked in lava... Did you forget that it burns? You lost your " ++ show treasureNbr ++ " treasures :(")
  | isLooseWaterState s = centerText "You're out of water. Staying hydrated is important. So you've lost :)"
  | isWinState s = centerText ("You managed to escape with a portal, well done!! You found " ++ show treasureNbr ++ " treasures!")
  | otherwise = Pictures [displayDesert s, showMagicCompass s]

centerText :: String -> Picture
centerText msg = Translate (-350) 0 $ Scale 0.15 0.15 $ Color black $ Text msg

bottomText :: String -> Picture
bottomText msg = Translate (-350) (-350) $ Scale 0.1 0.1 $ Color black $ Text msg


displayDesert :: DesertExplorerGameState -> Picture
displayDesert st =
  let DesertExplorerGameState{..} = st
      visible = getVisibleZone desert playerPos vision
      cellSize = 50
      spacing = 0
      fromCell = fromIntegral (cellSize + spacing)

      offsetX = - (fromIntegral (length (head visible)) * fromCell / 2)
      offsetY = - (fromIntegral (length visible) * fromCell / 2)

      nRows = length visible
      cellPictures =
        [ let cellPic = pictureOfCell cellSize 0 0 tile
              tx = fromIntegral x * fromCell
              ty = fromIntegral (nRows - 1 - y) * fromCell
              isPlayerHere = pos == playerPos
              playerPic = if isPlayerHere
                            then Color black $ Translate (fromIntegral cellSize / 2) (fromIntegral cellSize / 2) $ circleSolid (fromIntegral cellSize / 3)
                            else Blank
          in Translate tx ty (Pictures [cellPic, playerPic])
        | (y, row) <- zip [0..] visible
        , (x, (tile, pos)) <- zip [0..] row
        ]
  in Translate offsetX offsetY $ Pictures cellPictures


showMagicCompass :: DesertExplorerGameState -> Picture
showMagicCompass DesertExplorerGameState{..} = bottomText $
  "Player position: " ++ show playerPos ++
  " | Remaining water: " ++ show water ++
  " | Treasures found: " ++ show treasureNbr ++
  " | Nearest water: " ++ show nearestWater ++
  " | Nearest portal: " ++ show nearestPortal ++
  " | Nearest desert: " ++ show nearestDesert


treasureColor :: Color
treasureColor = makeColor 1.0 0.84 0.0 1.0

cellShape :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi
 = let  cs      = fromIntegral cellSize
        posX    = fromIntegral posXi
        posY    = fromIntegral posYi
        x1      = posX
        x2      = posX + cs
        y1      = posY
        y2      = posY + cs
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]


pictureOfCell :: Int -> Int -> Int -> GameTile -> Picture
pictureOfCell cellSize posX posY cell
 = case cell of
        Desert -> Color yellow  (cellShape cellSize posX posY)
        Lava   -> Color red     (cellShape cellSize posX posY)
        Water  -> Color blue    (cellShape cellSize posX posY)
        Treasure -> Color treasureColor (cellShape cellSize posX posY)
        Portal -> Color violet  (cellShape cellSize posX posY)