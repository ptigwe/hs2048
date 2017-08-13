module GameModel where

import Data.List

data Tile
  = Number Int
  | Empty
  deriving (Show, Eq)

newtype Grid =
  Grid [[Tile]]

data Progress
  = InProgress
  | GameOver
  | Won

data GameState = GameState
  { grid :: Grid
  , score :: Int
  , gameProgress :: Progress
  }

gridSize :: Int
gridSize = 4

readTile :: (Int, Int) -> Grid -> Tile
readTile (i, j) (Grid g) = (g !! j) !! i

setTile :: (Int, Int) -> Grid -> Tile -> Grid
setTile (i, j) (Grid g) t = Grid $ take j g ++ [nr] ++ drop (j + 1) g
  where
    r = g !! j
    nr = take i r ++ [t] ++ drop (i + 1) r

tileToInt :: Tile -> Int
tileToInt (Number n) = n
tileToInt Empty = 0

intToTile :: Int -> Tile
intToTile 0 = Empty
intToTile n = Number n

tilesWithCoordinates :: Grid -> [(Tile, Int, Int)]
tilesWithCoordinates (Grid g) =
  concat .
  zipWith (\j r -> map (\(t, i) -> (t, i, j)) r) [0 .. (gridSize - 1)] .
  map (\r -> zip r [0 .. (gridSize - 1)]) $
  g

rotateGrid :: Grid -> Grid
rotateGrid (Grid g) = Grid (map reverse . transpose $ g)

emptyGrid :: Grid
emptyGrid = Grid $ replicate gridSize . replicate gridSize $ Empty

defaultGame :: GameState
defaultGame = GameState {grid = emptyGrid, score = 0, gameProgress = InProgress}

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)
