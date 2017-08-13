{-# LANGUAGE OverloadedStrings #-}

module Rendering where

import GameModel
import Miso
import Miso.String (MisoString, ms)
import qualified Miso.String as S

black :: MisoString
black = "rgb(0,0,0)"

rgb :: Int -> Int -> Int -> MisoString
rgb r g b = S.pack $ "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

rgba :: Int -> Int -> Int -> Float -> MisoString
rgba r g b a =
  S.pack $ "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ show a ++ ")"

tileSize :: Float
tileSize = 106.25

tileMargin :: Float
tileMargin = 15

tileColor :: Tile -> MisoString
tileColor tile =
  case tile of
    Number 2 -> rgb 238 228 218
    Number 4 -> rgb 237 224 200
    Number 8 -> rgb 242 177 121
    Number 16 -> rgb 245 149 99
    Number 32 -> rgb 246 124 95
    Number 64 -> rgb 246 94 59
    Number 128 -> rgb 237 207 114
    Number 256 -> rgb 237 204 97
    Number 512 -> rgb 237 200 80
    Number 1024 -> rgb 237 197 63
    Number 2048 -> rgb 237 194 46
    _ -> rgba 238 228 218 0.35 -- empty tile

tileTextColor :: Tile -> MisoString
tileTextColor tile =
  case tile of
    Number n ->
      if n >= 8
        then rgb 249 246 242
        else rgb 119 110 101
    _ -> black

tileTextSize :: Tile -> Float
tileTextSize tile =
  case tile of
    Number 128 -> 45
    Number 256 -> 45
    Number 512 -> 45
    Number 1024 -> 35
    Number 2048 -> 35
    _ -> 55 -- empty tile

tileTextStyle :: Tile -> [(MisoString, MisoString)]
tileTextStyle tile =
  [ ("typeface", "'Helvetica Neue', Arial, sans-serif")
  , ("height", ms . show . tileTextSize $ tile)
  , ("color", tileTextColor tile)
  ]

gameOverOverlayStyle :: [(MisoString, MisoString)]
gameOverOverlayStyle = tileTextStyle (Number 2)

wonOverlayStyle :: [(MisoString, MisoString)]
wonOverlayStyle = tileTextStyle (Number 16)

gameOverOverlayColor :: MisoString
gameOverOverlayColor = rgba 238 228 218 0.73

wonOverlayColor :: MisoString
wonOverlayColor = rgba 237 194 46 0.5

wonMessage :: MisoString
wonMessage = "You won!"

display :: Model -> View Action
display model =
  div_
    [class_ "container"]
    [ script_
        [ src_ "http://gabrielecirulli.github.io/2048/style/main.css"
        , type_ "text/css"
        ]
        []
    , displayHeading model
    , displayIntro
    , displayGame model
    ]
