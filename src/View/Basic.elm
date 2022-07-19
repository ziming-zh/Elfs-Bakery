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

transparentTxt : Int -> Int -> Int -> Float -> String -> String -> Html Msg
transparentTxt x y size opa color txt = 
    div
        [ style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "font-size" (String.fromInt size ++ "px")
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "color" color
        , style "opacity" (String.fromFloat opa)
        ]
        [ text txt ]

renderTxt : (Int,Int) -> Int -> String -> String -> Float -> Html Msg
renderTxt (x,y) size color txt opa = 
    div
        [ style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "position" "absolute"
        , style "font-size" (String.fromInt size ++ "px")
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "color" color
        , style "opacity" (String.fromFloat opa)
        ]
        [ text txt ]

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

renderImg : String -> (Float,Float) -> Float -> Html Msg
renderImg link (x,y) opa = 
    Html.img
            [ HtmlAttr.src link
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat y ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.333 ++ ")")
            , HtmlAttr.style "opacity" (String.fromFloat opa)
            ][] 