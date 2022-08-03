module View.Basic exposing (rectRender,circleRender,renderButton,renderButtonColor,renderButtonRotate,renderChoiceButton,renderTxt, setWidth, setLength)

{-| This library defines many functions to draw basic things.

# Functions
@docs rectRender,circleRender,renderButton,renderButtonColor,renderButtonRotate,renderChoiceButton,renderTxt

# Data Type
@docs setWidth, setLength
-}

import Canvas exposing (Renderable)
import Canvas exposing (shapes)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..))
import Canvas.Settings exposing (fill,stroke)
import Color
import Canvas exposing (rect)
import Canvas exposing (circle)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Html.Events exposing (onMouseOver)
{-| length of a grid
-}
setLength : Float
setLength = 50.0
{-| width of the valve and wall
-}
setWidth : Float 
setWidth = 4.0


{-| This function draws a rectangle according to the
        position (x,y), width, hight and color. 
-}
rectRender : Float -> Float -> Float -> Float -> Color.Color -> Renderable
rectRender x y width height color= 
    shapes
        [ fill color ]
        [ rect ( x, y) width height ]

{-| This function draws a circle according to the 
        position (x,y), radius and color.
-}
circleRender : Float -> Float -> Float -> Color.Color -> Renderable
circleRender x y radius color =
    shapes
        [ stroke color]
        [ circle ( x, y) radius ]

{-| This function writes txt on the screen according to the 
        position (x,y), (font)-size, opa(opacity), color, txt(content).
-}
renderTxt : (Int,Int) -> Int -> String -> String -> Float -> Html Msg
renderTxt (x,y) size color txt opa = 
    div
        [ style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "font-size" (String.fromInt size ++ "px")
        , style "font-family" "Times New Romans"
        , style "color" color
        , style "opacity" (String.fromFloat opa)
        ]
        [ text txt ]

{-| This function draws a button on the screen according to the 
        position (x,y), msg(message), opa(opacity), color, txt(content).
-}
renderButton : String -> Msg -> (Int,Int) -> Float -> (Int,Int) -> String -> Html Msg
renderButton txt msg (x,y) opa (w,h) color =
    button
      [   style "background" "#4472C4"
        , style "border" "0"
        , style "color" color
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "line-height" "45px"
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        , style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "opacity" (String.fromFloat opa)
        , onClick msg
        ]
        [ text txt ]
{-| This function draws a colored button on the screen according to the 
        position (x,y), msg(message), opa(opacity), color, txt(content) and colorback(background color).
-}
renderButtonColor : String -> String -> Msg -> (Int,Int) -> Float -> (Int,Int) -> String -> Html Msg
renderButtonColor colorback txt msg (x,y) opa (w,h) color =
    button
      [   style "background" colorback
        , style "border" "0"
        , style "color" color
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "line-height" "45px"
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        , style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "opacity" (String.fromFloat opa)
        , onClick msg
        ]
        [ text txt ]
         
{-| This function renders a colored button on the screen which has 2 Msgs: click message and mouseover message.
-}
renderChoiceButton: String -> String -> Msg -> Msg->(Int,Int) -> Float -> (Int,Int) -> String -> Html Msg
renderChoiceButton colorback txt clickMsg mouseMsg (x,y) opa (w,h) color =
    button
      [   style "background" colorback
        , style "border" "0"
        , style "color" color
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "line-height" "45px"
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        , style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "opacity" (String.fromFloat opa)
        , onClick clickMsg
        , onMouseOver mouseMsg
        ]
        [ text txt ]

{-| This function draws a colored rotated button on the screen according to the 
        position (x,y), msg(message), opa(opacity), color, txt(content) and colorback(background color).
-}
renderButtonRotate : String -> String -> Msg -> (Int,Int) -> Float -> (Int,Int) -> String -> Html Msg
renderButtonRotate colorback txt msg (x,y) opa (w,h) color =
    button
      [   style "background" colorback
        , style "border" "0"
        , style "color" color
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "line-height" "45px"
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        , style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "opacity" (String.fromFloat opa)
        , style "transform-origin" "0 0"
        , style "transform" "rotate(270deg)"
        , onClick msg
        ]
        [ text txt ]
