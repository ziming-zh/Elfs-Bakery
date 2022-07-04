module View.Basic exposing (..)
import Canvas exposing (Renderable)
import Canvas exposing (shapes)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..))
import Canvas.Settings exposing (fill,stroke)
import Canvas.Texture
import Color
import Canvas exposing (rect)
import Canvas exposing (circle)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))

setLength : Float
setLength = 50.0

setWidth : Float 
setWidth = 4.0


rectRender : Float -> Float -> Float -> Float -> Color.Color -> Renderable
rectRender x y width height color= 
    shapes
        [ fill color ]
        [ rect ( x, y) width height ]

circleRender : Float -> Float -> Float -> Color.Color -> Renderable
circleRender x y radius color =
    shapes
        [ stroke color]
        [ circle ( x, y) radius ]

renderTxt : Int -> Int -> Int -> String -> String -> Html Msg
renderTxt x y size color txt = 
    div
        [ style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "font_size" (String.fromInt size ++ "px")
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "color" color
        ]
        [ text txt ]

renderButton : String -> Msg -> Int -> Int -> String -> Html Msg
renderButton txt msg x y color =
    button
      [   style "background" "#4472C4"
        , style "border" "0"
        , style "color" color
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "line-height" "45px"
        , style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , onClick msg
        ]
        [ text txt ]
