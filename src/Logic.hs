{-# LANGUAGE RecordWildCards #-}

module Logic where

import Data.Maybe
import GameModel
import InputModel
import Miso

groupedByTwo :: Eq a => [a] -> [[a]]
groupedByTwo [x] = [[x]]
groupedByTwo [x, y] =
  if x == y
    then [[x, y]]
    else [[x], [y]]
groupedByTwo (x:y:xs) =
  if x == y
    then [x, y] : groupedByTwo xs
    else [x] : groupedByTwo (y : xs)

slideRow :: [Tile] -> ([Tile], Int)
slideRow r =
  ( take
      gridSize
      (map (intToTile . sum . map tileToInt) grouped ++ replicate gridSize Empty)
  , sum . map tileToInt . concat . filter (\x -> length x > 1) $ grouped)
  where
    grouped = groupedByTwo . filter (/= Empty) $ r

rotatedGrid :: Direction -> Grid -> Grid
rotatedGrid D = rotateGrid
rotatedGrid R = rotateGrid . rotateGrid
rotatedGrid U = rotateGrid . rotateGrid . rotateGrid
rotatedGrid _ = id

slidGrid :: Direction -> Grid -> Grid
slidGrid U = rotateGrid
slidGrid R = rotateGrid . rotateGrid
slidGrid D = rotateGrid . rotateGrid . rotateGrid
slidGrid _ = id

slideGrid :: Direction -> Grid -> (Grid, Int)
slideGrid None grid = (grid, 0)
slideGrid dir grid = (newGrid, scoreGained)
  where
    rowsWithScores = map slideRow . (\(Grid h) -> h) . rotatedGrid dir $ grid
    slidRotatedGrid = Grid (map fst rowsWithScores)
    scoreGained = sum . map snd $ rowsWithScores
    newGrid = slidGrid dir slidRotatedGrid

gameLost :: Grid -> Bool
gameLost g = (g /= emptyGrid) && all (== g) [up, down, left, right]
  where
    up = fst . slideGrid U $ g
    down = fst . slideGrid D $ g
    left = fst . slideGrid L $ g
    right = fst . slideGrid R $ g

lose :: GameState -> GameState
lose gameState = gameState {gameProgress = GameOver}

win :: GameState -> GameState
win gameState = gameState {gameProgress = Won}

tile2Probability :: Float
tile2Probability = 0.9

newTile :: Float -> Tile
newTile x
  | x < tile2Probability = Number 2
  | otherwise = Number 4

emptyTiles :: Grid -> [(Int, Int)]
emptyTiles =
  map (\(_, i, j) -> (i, j)) .
  filter (\(t, _, _) -> t == Empty) . tilesWithCoordinates

newTileIndex :: Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
  case emptyTileIndices of
    [] -> Nothing
    _ -> Just (emptyTileIndices !! idx)
  where
    emptyTileIndices = emptyTiles g
    idx = (floor . (* x) . fromIntegral . length $ emptyTileIndices) :: Int

placeRandomTile :: Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState@GameState {..} =
  if isNothing tileIndex
    then gameState
    else gameState
         {grid = setTile (fromMaybe (0, 0) tileIndex) grid $ newTile float2}
  where
    tileIndex = newTileIndex float1 grid

newGame :: GameState -> GameState
newGame state@GameState {..} =
  placeRandomTile (randomFloats !! 0) (randomFloats !! 1) .
  placeRandomTile (randomFloats !! 2) (randomFloats !! 3) $
  state

updateGameState :: Action -> GameState -> Effect Action GameState
updateGameState NewGame state = noEff (newGame state)
updateGameState (GetArrows arr) state = noEff nState
  where
    nState = state {direction = toDirection arr, count = 1 + count state}
updateGameState _ state = noEff state
