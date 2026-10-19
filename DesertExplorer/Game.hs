{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

{-
                                       TITLE : Desert Explorer
    Name : Moussa Tallih
    Course : Functional Programming
    Date : 03/06/2025
-}

module Game where

import qualified Data.Set as Set
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import System.Random (mkStdGen, randomR)


-- Constants

waterPerMove :: Int
waterPerMove = 5

-- Maximum distance the magic compass searches (the map is infinite)
compassRange :: Int
compassRange = 30

type Command = String

class GameState s where
    nextState :: s -> Command -> Maybe (Either String s)
    isLooseWaterState :: s -> Bool
    isLooseLavaState :: s -> Bool
    isWinState :: s -> Bool
    isFinalState :: s -> Bool

-- To "boot" a game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either String s

--
-- Desert Explorer
--

-- Types used

type Position = (Int, Int)

data GameTile = Desert | Treasure | Water | Portal | Lava deriving (Show, Eq)

type GameMap = [[(GameTile, Position)]]


-- Parse tool

readMove :: String -> Maybe Char
readMove [c] | c `elem` "WASD" = Just c
readMove _ = Nothing

-- Map tools

updateAt :: Int -> a -> [a] -> [a]
updateAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs

setGameTile :: Position -> GameTile -> GameMap -> GameMap
setGameTile (x, y) newTile gameMap = updateAt y newRow gameMap
  where
    row = gameMap !! y
    (_, pos) = row !! x
    newRow = updateAt x (newTile, pos) row

getCell :: Position -> GameMap -> (GameTile, Position)
getCell (x, y) gameMap = gameMap !! y !! x

tileAt :: Position -> GameMap -> GameTile
tileAt pos = fst . getCell pos

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

getVisibleZone :: GameMap -> Position -> Int -> GameMap
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

renderMap :: GameMap -> Position -> Int -> String
renderMap grid playerPos vision =
  let visible = getVisibleZone grid playerPos vision
  in unlines [ [ renderCell playerPos cell | cell <- row ] | row <- visible ]

showDistance :: Maybe Int -> String
showDistance = maybe ("> " ++ show compassRange) show

---


-- Desert Explorer Game Configuration and State

data DesertExplorerConfig = DesertExplorerConfig { sight :: Int, maxWaterCapacity :: Int , seedG :: Int,
                                                    treasureChance :: Int,   waterChance :: Int, portalChance :: Int,
                                                    singleLavaChance :: Int, surroundedLavaChance :: Int}

data DesertExplorerGameState = DesertExplorerGameState { playerPos :: Position, water :: Int,
                                                         desert :: GameMap, treasureNbr :: Int,
                                                         waterBound :: Int, nearestWater :: Maybe Int,
                                                         nearestPortal :: Maybe Int, nearestDesert :: Maybe Int,
                                                         vision :: Int }

instance GameState DesertExplorerGameState where
    nextState st input = fmap (doAction st) (readMove input)
    isLooseWaterState st = water st <= 0
    isLooseLavaState st = isLava (playerPos st) (desert st)
    isWinState st = isPortal (playerPos st) (desert st)
    isFinalState s = isLooseLavaState s || isLooseWaterState s || isWinState s

instance TerminalGame DesertExplorerGameState DesertExplorerConfig where
    initialState config@DesertExplorerConfig{..}
        | any (< 0) [treasureChance, waterChance, portalChance, singleLavaChance, surroundedLavaChance] =
            Left "Invalid configuration: chances must be positive"
        | treasureChance > 100 = Left "Invalid configuration: t > 100"
        | sight < 0 = Left "Invalid configuration: sight must be positive"
        | maxWaterCapacity <= 0 = Left "Invalid configuration: water capacity must be > 0"
        | w + p + l > 100 = Left "Invalid configuration: w + p + l > 100"
        | w + p + ll > 100 = Left "Invalid configuration: w + p + ll > 100"
        | otherwise = Right $ magicCompass $
            DesertExplorerGameState (0, 0) maxWaterCapacity (generateMap config) 0 maxWaterCapacity
                                    Nothing Nothing Nothing sight
        where w = waterChance
              p = portalChance
              l = singleLavaChance
              ll = surroundedLavaChance


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
        , "Nearest water      : " ++ showDistance nearestWater
        , "Nearest portal     : " ++ showDistance nearestPortal
        , "Nearest desert     : " ++ showDistance nearestDesert
        ]

--

-- Map Generation Tools

-- Unique seed per tile (Cantor pairing of the coordinates, mixed with the global seed)
tileSeed :: Int -> Position -> Int
tileSeed seed (x, y) = seed * 1000003 + (x + y) * (x + y + 1) `div` 2 + y

-- The map is generated lazily, row by row and left to right. Each tile knows the tile above it and
-- the tile to its left, so the lava chance can depend on whether lava is already adjacent.
generateMap :: DesertExplorerConfig -> GameMap
generateMap config = rows
  where
    rows = zipWith genRow [0 ..] (repeat Nothing : map (map (Just . fst)) rows)
    genRow y ups = row
      where
        row = zipWith3 (genTile y) [0 ..] ups lefts
        lefts = Nothing : map (Just . fst) row
    genTile y x up left
      | (x, y) == (0, 0) = (Desert, (0, 0))
      | otherwise = (genGameTile config (x, y) (Just Lava `elem` [up, left]), (x, y))

genGameTile :: DesertExplorerConfig -> Position -> Bool -> GameTile
genGameTile DesertExplorerConfig{..} pos lavaAdjacent
    | chance <= w = Water
    | chance <= w + p = Portal
    | chance <= w + p + lavaChance = Lava
    | treasureRoll <= treasureChance = Treasure
    | otherwise = Desert
  where
    w = waterChance
    p = portalChance
    lavaChance = if lavaAdjacent then surroundedLavaChance else singleLavaChance
    (chance, gen) = randomR (1, 100 :: Int) (mkStdGen (tileSeed seedG pos))
    (treasureRoll, _) = randomR (1, 100 :: Int) gen

--


-- Check if a position is of a specific tile type

isLava :: Position -> GameMap -> Bool
isLava pos gameMap = tileAt pos gameMap == Lava

isPortal :: Position -> GameMap -> Bool
isPortal pos gameMap = tileAt pos gameMap == Portal

isWater :: Position -> GameMap -> Bool
isWater pos gameMap = tileAt pos gameMap == Water

isDesert :: Position -> GameMap -> Bool
isDesert pos gameMap = tileAt pos gameMap == Desert

-- Movement tools

outOfMap :: String
outOfMap = "You can't leave the map"

goDown :: Position -> Either String Position
goDown (x, y) = Right (x, y + 1)

goUp :: Position -> Either String Position
goUp (x, y)
  | y - 1 < 0 = Left outOfMap
  | otherwise = Right (x, y - 1)

goLeft :: Position -> Either String Position
goLeft (x, y)
  | x - 1 < 0 = Left outOfMap
  | otherwise = Right (x - 1, y)

goRight :: Position -> Either String Position
goRight (x, y) = Right (x + 1, y)

movePlayer :: Char -> Position -> Either String Position
movePlayer 'W' = goUp
movePlayer 'S' = goDown
movePlayer 'D' = goRight
movePlayer 'A' = goLeft
movePlayer c   = const (Left ("Unknown move: " ++ [c]))

--

-- Game Logic

doEventTile :: DesertExplorerGameState -> GameTile -> DesertExplorerGameState
doEventTile st Treasure =
    st { treasureNbr = treasureNbr st + 1, desert = setGameTile (playerPos st) Desert (desert st) }
doEventTile st Water = st { water = waterBound st }
doEventTile st _ = st

doEvent :: DesertExplorerGameState -> DesertExplorerGameState
doEvent st = doEventTile st (tileAt (playerPos st) (desert st))

doAction :: DesertExplorerGameState -> Char -> Either String DesertExplorerGameState
doAction currentState input = do
  newPos <- movePlayer input (playerPos currentState)
  let moved = currentState { playerPos = newPos, water = water currentState - waterPerMove }
  return $ magicCompass (doEvent moved)

--

-- Magic Compass / BFS Algorithm

magicCompass :: DesertExplorerGameState -> DesertExplorerGameState
magicCompass st =
  st { nearestWater  = distanceTo isWater
     , nearestPortal = distanceTo isPortal
     , nearestDesert = distanceTo isDesert
     }
  where distanceTo = bfs (desert st) (playerPos st)

getNeighbors :: Position -> [Position]
getNeighbors (x, y) =
  filter (\(nx, ny) -> nx >= 0 && ny >= 0)
    [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Number of moves to the nearest tile (other than the current one) satisfying cond,
-- without walking through lava. Nothing when no such tile is within compassRange.
bfs :: GameMap -> Position -> (Position -> GameMap -> Bool) -> Maybe Int
bfs grid start cond = go (Seq.singleton (start, 0)) (Set.singleton start)
  where
    go :: Seq (Position, Int) -> Set.Set Position -> Maybe Int
    go queue visited = case viewl queue of
      EmptyL -> Nothing
      (p, d) :< rest
        | p /= start && cond p grid -> Just d
        | d >= compassRange -> go rest visited
        | otherwise ->
            let next = [ n | n <- getNeighbors p, Set.notMember n visited, not (isLava n grid) ]
                visited' = foldr Set.insert visited next
                queue' = foldl (|>) rest [ (n, d + 1) | n <- next ]
            in go queue' visited'
