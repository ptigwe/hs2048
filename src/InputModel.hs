{-# LANGUAGE RecordWildCards #-}

module InputModel where

import Miso
import Touch

data Direction
  = U
  | D
  | L
  | R
  | None
  deriving (Show, Eq)

data Action
  = Init
  | NewGame
  | GetArrows Arrows
  | Continue
  | Sync
  | TouchStart TouchEvent
  | TouchEnd TouchEvent
  | NoOp

toDirection :: Arrows -> Direction
toDirection arr@Arrows {..} =
  case (arrowX, arrowY) of
    (-1, 0) -> L
    (1, 0) -> R
    (0, -1) -> D
    (0, 1) -> U
    _ -> None
