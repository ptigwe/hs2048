module GameModel where

import Data.List
import InputModel

data Tile
  = Number Int
  | Empty
  deriving (Eq)

instance Show Tile where
  show (Number n) = show n
  show Empty = "-"

newtype Grid =
  Grid [[Tile]]
  deriving (Eq)

instance Show Grid where
  show (Grid x) = unlines . map show $ x

data Progress
  = InProgress
  | GameOver
  | Won
  deriving (Show, Eq)

data GameState = GameState
  { grid :: Grid
  , drawGrid :: Grid
  , score :: Int
  , bestScore :: Int
  , gameProgress :: Progress
  , direction :: Direction
  , randomSeed :: Int
  } deriving (Show, Eq)

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
defaultGame =
  GameState
  { grid = emptyGrid
  , drawGrid = emptyGrid
  , score = 0
  , bestScore = 0
  , gameProgress = InProgress
  , direction = None
  , randomSeed = 0
  }
