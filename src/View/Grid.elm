module View.Grid exposing (..)

import Array
import Canvas exposing (Renderable)
import Color
import Grid exposing (Grid, GridType(..), Grids)
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)
import String exposing (indexes)
import View.Basic exposing (rectRender, setLength, setWidth)

import Message exposing (SpecialType(..),Sstate(..))
renderGrids : Grids -> List Renderable
renderGrids grids =
    List.map (\y -> Array.toList (Array.map (\x -> drawGrid x) y)) (Array.toList grids)
        |> List.foldl List.append []


drawGrid : Grid -> Renderable
drawGrid grid =
    let
        posy =
            toFloat grid.pos.x * setLength

        posx =
            toFloat grid.pos.y * setLength

        r =
            254 - Maybe.withDefault 0 grid.distance * 1

        g =
            248 - Maybe.withDefault 0 grid.distance * 4

        b =
            231 - Maybe.withDefault 0 grid.distance * 1

        color =
            case grid.gridtype of
                Vacant ->
                    Color.rgb255 r g b

                Exit ->
                    Color.lightGray

                Paint paint ->
                    paint.color
    in
    rectRender posx posy setLength setLength color


drawStype : Grid -> List (Html msg)
drawStype grid =
    case grid.stype of
        Just a ->
            let
                pic =
                    case a.content of
                        Chocolate ->
                            HtmlAttr.src "./assets/chocolate_grid.png"

                        Vanilla ->
                            HtmlAttr.src "./assets/vanilla_grid.png"
            in
            [ Html.img
                [ pic
                , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
                , HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "left" (String.fromFloat ((toFloat grid.pos.y) * setLength) ++ "px")
                , HtmlAttr.style "top" (String.fromFloat ((toFloat grid.pos.x) * setLength) ++ "px")
                ]
                []
            ]

        Nothing ->
            []
renderStypes : Grids -> List (Html msg)
renderStypes grids =
    List.concat (List.map (\y -> Array.toList (Array.map (\x -> drawStype x) y)) (Array.toList grids)
        |> List.foldl List.append [])