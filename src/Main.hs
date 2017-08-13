-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)

import GameModel
import Logic
import Rendering

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = 0 -- initial model
    update = updateModel -- update function
    view = display -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do putStrLn "Hello World" >> pure NoOp
