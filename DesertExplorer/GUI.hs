{-# LANGUAGE RecordWildCards #-}

module GUI (runGUI) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Char (toUpper)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), playIO)
import Game


type GameVar = TVar DesertExplorerGameState

-- Esc closes the window (built into gloss)
runGUI :: DesertExplorerConfig -> IO ()
runGUI config =
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


drawWorldSTM :: GameVar -> IO Picture
drawWorldSTM gameVar = drawWorld <$> readTVarIO gameVar

updateWorldSTM :: Float -> GameVar -> IO GameVar
updateWorldSTM _ = return

handleInputSTM :: Event -> GameVar -> IO GameVar
handleInputSTM event gameVar = do
  atomically $ modifyTVar' gameVar (handleInput event)
  return gameVar


handleInput :: Event -> DesertExplorerGameState -> DesertExplorerGameState
handleInput (EventKey (Char c) Down _ _) state
  | c `elem` "wasdWASD" && not (isFinalState state) =
      case nextState state [toUpper c] of
        Just (Right newState) -> newState
        _ -> state
handleInput _ state = state


drawWorld :: DesertExplorerGameState -> Picture
drawWorld s@DesertExplorerGameState{..}
  | isLooseLavaState s = centerText ["You walked in lava... Did you forget that it burns?",
                                     "You lost your " ++ show treasureNbr ++ " treasures :("]
  | isLooseWaterState s = centerText ["You're out of water. Staying hydrated is important.",
                                      "So you've lost :)"]
  | isWinState s = centerText ["You managed to escape with a portal, well done!!",
                               "You found " ++ show treasureNbr ++ " treasures!"]
  | otherwise = Pictures [displayDesert s, showMagicCompass s]

textLines :: Float -> Float -> Float -> [String] -> Picture
textLines x y size msgs =
  Pictures [ Translate x (y - fromIntegral i * size * 180) $ Scale size size $ Color black $ Text msg
           | (i, msg) <- zip [0 :: Int ..] msgs ]

centerText :: [String] -> Picture
centerText = textLines (-350) 0 0.15

bottomText :: [String] -> Picture
bottomText = textLines (-480) (-310) 0.1


displayDesert :: DesertExplorerGameState -> Picture
displayDesert DesertExplorerGameState{..} =
  let visible = getVisibleZone desert playerPos vision
      cellSize = 50
      fromCell = fromIntegral cellSize
      nRows = length visible
      offsetX = - (fromIntegral (2 * vision + 1) * fromCell / 2)
      -- leave room for the compass text at the bottom
      offsetY = - (fromIntegral nRows * fromCell / 2) + 50

      cellPictures =
        [ let tx = fromIntegral x * fromCell
              ty = fromIntegral (nRows - 1 - y) * fromCell
              playerPic
                | pos == playerPos = Color black $ Translate (fromCell / 2) (fromCell / 2) $ circleSolid (fromCell / 3)
                | otherwise = Blank
          in Translate tx ty (Pictures [pictureOfCell cellSize tile, playerPic])
        | (y, row) <- zip [0 :: Int ..] visible
        , (x, (tile, pos)) <- zip [0 :: Int ..] row
        ]
  in Translate offsetX offsetY $ Pictures cellPictures


showMagicCompass :: DesertExplorerGameState -> Picture
showMagicCompass DesertExplorerGameState{..} = bottomText
  [ "Player position: " ++ show playerPos ++
    " | Remaining water: " ++ show water ++
    " | Treasures found: " ++ show treasureNbr
  , "Nearest water: " ++ showDistance nearestWater ++
    " | Nearest portal: " ++ showDistance nearestPortal ++
    " | Nearest desert: " ++ showDistance nearestDesert
  ]


treasureColor :: Color
treasureColor = makeColor 1.0 0.84 0.0 1.0

cellShape :: Int -> Picture
cellShape cellSize =
  let cs = fromIntegral cellSize
  in Polygon [(0, 0), (0, cs), (cs, cs), (cs, 0)]

pictureOfCell :: Int -> GameTile -> Picture
pictureOfCell cellSize cell = Color (tileColor cell) (cellShape cellSize)

tileColor :: GameTile -> Color
tileColor Desert   = yellow
tileColor Lava     = red
tileColor Water    = blue
tileColor Treasure = treasureColor
tileColor Portal   = violet
