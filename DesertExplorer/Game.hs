{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


module  Game where
import Graphics.Gloss
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




{-

                                       TITLE : Desert Explorer                                   
    Name : Moussa Tallih
    Course : Functional Programming
    Date : 03/06/2025
-}


-- Constants
waterPerMove :: Int
waterPerMove = 5

type Command = String

class GameState s where
    nextState :: s -> Command -> Maybe (Either String s)
    isLooseWaterState :: s -> Bool
    isLooseLavaState :: s -> Bool
    isWinState :: s -> Bool
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either String s

-- 
-- Desert Explorer 
-- 

-- Types used 

type Position = (Int, Int)

data GameTile = Desert | Treasure | Water | Portal | Lava deriving (Show, Eq)


-- Parse tool

readMaybe' :: String -> Maybe Char
readMaybe' ['A'] = Just 'A'
readMaybe' ['D'] = Just 'D'
readMaybe' ['W'] = Just 'W'
readMaybe' ['S'] = Just 'S'
readMaybe' _ = Nothing

-- Map tools


updateAt :: Int -> a -> [a] -> [a]
updateAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs

setGameTile :: Position -> GameTile -> [[(GameTile, Position)]] -> [[(GameTile, Position)]]
setGameTile (x, y) newTile gameMap =
  updateAt y newRow gameMap
  where
    row = gameMap !! y
    (oldTile, pos) = row !! x
    newRow = updateAt x (newTile, pos) row


getCell :: Position -> [[(GameTile, Position)]] ->  (GameTile, Position)
getCell (x, y) gameMap =
  let row = getRow y gameMap
  in row !! x

getRow :: Int -> [[(GameTile, Position)]] ->  [(GameTile, Position)]
getRow = flip (!!)

---  
----- Tools to Show
---
getWindowBounds :: Position -> Int -> (Int, Int, Int, Int)
getWindowBounds (px, py) vision =
  let minX = max 0 (px - vision)
      minY = max 0 (py - vision)
      maxX = minX + 2 * vision
      maxY = minY + 2 * vision
  in (minX, maxX, minY, maxY)


getVisibleZone :: [[(GameTile, Position)]] -> Position -> Int -> [[(GameTile, Position)]]
getVisibleZone grid (px, py) vision =
  let (minX, maxX, minY, maxY) = getWindowBounds (px, py) vision
  in [ [ grid !! y !! x | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]

tileChar :: GameTile -> Char
tileChar Lava     = 'L'
tileChar Water    = 'W'
tileChar Treasure = 'T'
tileChar Portal   = 'P'
tileChar Desert   = '.'

renderCell :: Position -> (GameTile, Position) -> Char
renderCell playerPos (tile, pos)
  | pos == playerPos = '@'
  | otherwise        = tileChar tile

renderMap :: [[(GameTile, Position)]] -> Position -> Int -> String
renderMap grid playerPos vision =
  let visible = getVisibleZone grid playerPos vision
  in unlines [ [ renderCell playerPos cell | cell <- row ] | row <- visible ]

---


-- Desert Explorer Game Configuration and State 

data DesertExplorerConfig = DesertExplorerConfig { sight :: Int, maxWaterCapacity :: Int , seedG :: Int,
                                                    treasureChance :: Int,   waterChance :: Int, portalChance :: Int,
                                                    singleLavaChance :: Int, surroundedLavaChance :: Int}

data DesertExplorerGameState = DesertExplorerGameState { playerPos :: Position, water::Int,
                                                         desert :: [[(GameTile,Position)]], treasureNbr :: Int,
                                                         waterBound :: Int, nearestWater::Int,
                                                          nearestPortal::Int, nearestDesert :: Int, vision :: Int}

instance GameState DesertExplorerGameState where
    nextState st input = fmap (doAction st) (readMaybe' input)
    isLooseWaterState DesertExplorerGameState { water = waterStock, ..} = waterStock <= 0
    isLooseWaterState _ = False
    isLooseLavaState DesertExplorerGameState {playerPos = currentPos, desert = gameMap, ..} = isLava currentPos gameMap
    isLooseLavaState _ = False
    isWinState DesertExplorerGameState {playerPos = currentPos, desert = gameMap, ..} = isPortal currentPos gameMap
    isWinState _ = False
    isFinalState s = isLooseLavaState s || isLooseWaterState s  || isWinState s

instance TerminalGame DesertExplorerGameState DesertExplorerConfig where
    initialState config@DesertExplorerConfig{..}
        | w + p + l >= 100 = Left "Invalid configuration: w + p + l > 100"
        | w  + p + ll >= 100 = Left "Invalid configuration: w + p + ll > 100"
        | otherwise =  Right $ DesertExplorerGameState (0,0) maxWaterCapacity gameMap 0 maxWaterCapacity 0 0 0 sight
        where w = waterChance
              p = portalChance
              l = singleLavaChance
              ll = surroundedLavaChance
              gameMap = [[ if (x, y) == (0, 0)
                            then (Desert, (0, 0))
                            else tempGenGameTile config (x, y)
                        | x <- [0..]] | y <- [0..]]


instance Show DesertExplorerGameState where
  show s@DesertExplorerGameState{..}
    | isLooseLavaState s = "You walked in lava... Did you forget that it burns?"
    | isLooseWaterState s = "You're out of water. Staying hydrated is important. So you've lost :)"
    | isWinState s = "You managed to escape with a portal, well done!!"
    | otherwise  = unlines
        [ renderMap desert playerPos vision
        , "Player position    : " ++ show playerPos
        , "Remaining water    : " ++ show water
        , "Treasures found    : " ++ show treasureNbr
        , "Nearest water      : " ++ show nearestWater
        , "Nearest portal     : " ++ show nearestPortal
        , "Nearest desert     : " ++ show nearestDesert
        ]

-- 

-- Map Generation Tools 

desertOrTreasure :: Int ->  Position  -> Int -> GameTile
desertOrTreasure treasureChance (x,y) seed
    | treasureChance > chance = Treasure
    | otherwise = Desert
    where
        seedUp = seed  + x * 100 + y
        (chance,_) = randomR (1, 100) (mkStdGen seedUp)


tempGenGameTile :: DesertExplorerConfig -> Position -> (GameTile, Position)
tempGenGameTile config pos@(x, y)
    | chance <=  w = (Water, pos)
    | chance <=  w + p = (Portal, pos)
    | chance <=  w + p + l = (Lava, pos)
    | chance <=  w + p + l + ll = (Lava, pos)
    | otherwise = (desertOrTreasure t pos seed, pos)
  where
    t = treasureChance config
    w = waterChance config
    p = portalChance config
    l = singleLavaChance config
    ll = surroundedLavaChance config
    seed = seedG config + x * 100 + y
    (chance, _) = randomR (1, 100) (mkStdGen seed)

-- 


-- Check if a position is of a specific tile type

isLava :: (Int, Int) -> [[(GameTile, Position)]] -> Bool
isLava (x, y) gameMap = (== Lava) . fst $ (gameMap !! y !! x)

isPortal :: (Int, Int) -> [[(GameTile, Position)]] -> Bool
isPortal (x, y) gameMap = (== Portal) . fst $ (gameMap !! y !! x)

isWater :: (Int, Int) -> [[(GameTile, Position)]] -> Bool
isWater (x, y) gameMap = (== Water) . fst $ (gameMap !! y !! x)

isDesert :: (Int, Int) -> [[(GameTile, Position)]] -> Bool
isDesert (x, y) gameMap = (== Desert) . fst $ (gameMap !! y !! x)

-- Movement tools

goDown :: Position -> Position
goDown (x,y) = (x, y+1)

goUp:: Position -> Either String Position
goUp (x, y) = if y -1 < 0
                then Left "Vous essayez de sortir des limites de la map"
                else Right (x, y-1)

goLeft :: Position -> Either String Position
goLeft (x, y) = if x -1 < 0
                then Left "Vous essayez de sortir des limites de la map"
                else Right (x-1, y)

goRight :: Position -> Position
goRight (x, y) = (x+1, y)

movePlayer :: Char -> Position -> Either String Position
movePlayer 'W' pos = goUp pos
movePlayer 'S' pos = Right $ goDown pos
movePlayer 'D' pos = Right $ goRight pos
movePlayer 'A' pos = goLeft pos
--

-- Game Logic

doEventTile currentState@DesertExplorerGameState{playerPos, desert, treasureNbr} Treasure =
    currentState { treasureNbr = treasureNbr + 1, desert = newMap}
  where
    newMap = setGameTile playerPos Desert desert

doEventTile currentState@DesertExplorerGameState{water, waterBound} Water =
    currentState { water = waterBound }

doEventTile currentState _  = currentState


doEvent currentState = doEventTile  currentState tileType
                        where (tileType,_) = getCell (playerPos currentState) (desert currentState)



doAction :: DesertExplorerGameState -> Char -> Either String DesertExplorerGameState
doAction currentState input =
  case movePlayer input (playerPos currentState) of
    Left err -> Left err
    Right newPos ->
      let tempState = currentState { playerPos = newPos, water = water currentState - waterPerMove }
          tempState2 = doEvent tempState
          newState = magicCompass tempState2
      in Right newState
--

-- Magic Compass / BFS Algorithm (Not really Working unfortunately)

magicCompass currentState@DesertExplorerGameState{nearestWater, nearestPortal, nearestDesert} =
  let newNearestWater = bfs (desert currentState) [playerPos currentState] [] isWater
      newNearestPortal = bfs (desert currentState) [playerPos currentState] [] isPortal
      newNearestDesert = bfs (desert currentState) [playerPos currentState] [] isDesert
  in currentState { nearestWater = length newNearestWater, nearestPortal = length newNearestPortal, nearestDesert = length newNearestDesert }

getNeighbors :: Position -> [Position]
getNeighbors (x, y) =
  filter (\(nx, ny) -> nx >= 0 && ny >= 0)
    [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

bfs :: [[(GameTile, Position)]]
    -> [Position]
    -> [Position]
    -> (Position -> [[(GameTile, Position)]] -> Bool)
    -> [Position]
bfs _ [] _ _ = []
bfs grid (p:ps) visited cond
  | p `elem` visited = bfs grid ps visited cond
  | isLava p grid = bfs grid ps (p : visited) cond  -- 
  | cond p grid = p : visited
  | otherwise =
      let next = filter (`notElem` visited) (getNeighbors p)
      in bfs grid (ps ++ next) (p : visited) cond
