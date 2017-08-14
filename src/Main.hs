-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import System.Random

import GameModel
import InputModel
import Logic
import Rendering

-- | Entry point for a miso application
main :: IO ()
main = do
  stdGen <- getStdGen
  let (seed, _) = random stdGen
  startApp App {model = defaultGame {randomSeed = seed}, ..}
  where
    initialAction = Init -- initial action to be executed on application load
    model = defaultGame -- initial model
    update = updateGameState -- update function
    view = display -- view function
    events = defaultEvents -- default delegated events
    subs = [arrowsSub GetArrows] -- empty subscription list
