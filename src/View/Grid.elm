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
    let
        listgrid =  Array.toList ( Array.map ( \x -> Array.toList x ) grids )
        listgrids = List.concat listgrid
        listdis = List.map  (
                            \x -> 
                                case x.distance of 
                                    Nothing -> 0 
                                    Just a -> a
                            ) listgrids
        mx = Basics.max (List.foldl Basics.max 0 listdis) 1
    in
        List.map (\y -> Array.toList (Array.map (\x-> drawGrid (Basics.toFloat mx) x) y)) (Array.toList grids)
        |> List.foldl List.append []


drawGrid : Float -> Grid -> Renderable
drawGrid mx grid =
    let
        posy = (toFloat grid.pos.x) * setLength
        posx = (toFloat grid.pos.y) * setLength
        r = (254-(Basics.toFloat (Maybe.withDefault 0 grid.distance))/mx * 20)/255
        g = (248-(Basics.toFloat (Maybe.withDefault 0 grid.distance))/mx * 80)/255
        b = (231-(Basics.toFloat(Maybe.withDefault 0 grid.distance))/mx * 20)/255
        color = 
            case grid.gridtype of
                Vacant -> Color.rgb r g b 
                Exit -> Color.lightGray
                Paint paint -> paint.color
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