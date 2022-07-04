module View.Pacman exposing (FanShape,viewFanShape)

import Debug exposing (toString)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray)
import Svg.Attributes exposing (zoomAndPan)
import Svg.Attributes exposing (transform)
import Color
type alias FanShape =
  { offset: Float
  , percentage: Float
  , color: String
  , pos: (Float, Float)
  }

viewBall : Model -> Player -> Html Msg
viewBall model player =
    -- drawcir ( Tuple.first ball.pos , Tuple.second ball.pos ) 15 "#FFEC8B"
    -- View.pacman model
    let
        (vx,vy) = player.vel
        neg num = 
            if num > 0 then 1
            else 0
        
        percentage = 
            if round (toFloat(round model.time)/2) == round (toFloat(round (model.time-1))/2)
                then 0.75
            else 0.9
        -- offset = atan (vy/vx) / (2*pi)+  1/2*(neg vx)+ (1-percentage)/2 +(-1/4)
        offset = 0
        fanshapes = FanShape (offset*100) (percentage*100) Color.red player.pos
    in
       viewFanShape fanshapes

viewFanShape : FanShape -> Html Msg
viewFanShape fanShape =
  let
    (x,y) = fanShape.pos
    strokeDashoffset_ = String.fromFloat <| 25.0 - fanShape.offset
    strokeDasharray_ = String.fromFloat fanShape.percentage ++ " " ++ (String.fromFloat <| 100.0 - fanShape.percentage)
  in
    circle
      [ cx (toString 0), cy (toString 0), r "15"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke fanShape.color, strokeWidth "30", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ 
      , transform ("translate("++(toString x)++","++(toString y)++") scale (0.6,0.6)")
     ]
      []

angle : Float
angle =
    degrees 40


