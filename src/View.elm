module View exposing (view)
import Model exposing (Model,updateGridsfromModel)
import Message exposing (Msg(..))
import Html exposing (Html, button, div, text)
import Canvas exposing (toHtml)
import View.Wall exposing (drawWall)
import Model exposing (Mapset,updateGridsfromModel)
import View.Basic exposing (rectRender)
import Color exposing (Color)
import Canvas exposing (clear)
import Canvas exposing (rect)
import Canvas exposing (shapes)
import Html.Attributes exposing (style)
import Canvas.Settings exposing (fill)
import View.Valve exposing (renderValves)
import View.Level exposing (renderLevel)
import DBFS exposing(get)
import Grid exposing(Grid)
import Array

view : Model -> Html Msg
view model =
    let
        level = List.head model.levels
        arraylist = Array.map ( \xx -> Array.toList xx ) model.updatedGrids
        list = List.concat (Array.toList arraylist)
        dis = List.map (\xx ->
            case xx.distance of 
                Just a -> a
                Nothing -> 1000 
            ) list
    in
    div
        [ ]
        (List.concat
        [[ toHtml (800 ,600)
            []
            (renderLevel model.wall model.valves (model.updatedGrids) model.player)
        ], List.map (\xx -> text ((String.fromInt xx)++" ")) dis])

-- view : Model -> Html msg
-- view  model =
--     let
--         width = 600
--         height = 300
--     in
--         Canvas.toHtml (width, height)
--             [ style "border" "1px solid black" ]
--             [ shapes [ fill Color.red ] [ rect (0, 0) width height ]
--             , renderSquare
--             ]

-- renderSquare =
--   shapes [ fill (Color.rgba 0 0 0 1) ]
--       [ rect (0, 0) 100 50 ]