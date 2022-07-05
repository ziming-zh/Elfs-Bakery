module View.Pacman exposing (fromPlayertoFanShape)

import Debug exposing (toString)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Player exposing (Player)
import Color
import Message exposing (Direction(..))
import View.Basic exposing (setLength)
import Canvas exposing (Renderable)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
-- import Canvas.Settings.Text exposing (..)

fromPlayertoFanShape : Player -> Renderable
fromPlayertoFanShape player =
  let
    color = Color.gray
    anglepair = 
      case player.dir of
        Right -> ((degrees 30),(degrees 330))
        Down -> ((degrees 120),(degrees 420))
        Left -> ((degrees 210),(degrees 510))
        Up -> ((degrees 300),(degrees 600))
        Stop ->  ((degrees 0),(degrees 360))
    -- anglepair = (0.0,0.1)

    posx = (toFloat player.pos.y)*setLength +setLength/2
    posy = (toFloat player.pos.x)*setLength  +setLength/2
  in
    renderPieSlice color (posx,posy) 20 (Tuple.first anglepair) (Tuple.second anglepair)



renderPieSlice : Color.Color -> (Float, Float) -> Float -> Float -> Float -> Renderable
renderPieSlice color (( x, y ) as center) radius startAngle endAngle =
    shapes [ fill color ]
        [ path center
            [ lineTo ( x + radius * cos startAngle, y + radius * sin startAngle )
            , lineTo ( x + radius * cos endAngle, y + radius * sin endAngle )
            , lineTo center
            ]
        , arc
            center
            radius
            { startAngle = startAngle
            , endAngle = endAngle
            , clockwise = True
            }
        ]

