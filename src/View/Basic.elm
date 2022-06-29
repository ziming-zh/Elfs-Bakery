module View.Basic exposing (..)
import Canvas exposing (Renderable)
import Canvas exposing (shapes)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..))
import Canvas.Settings exposing (fill,stroke)
import Canvas.Texture
import Color
import Canvas exposing (rect)
import Canvas exposing (circle)

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

