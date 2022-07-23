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


drawStype : Int -> Int -> Grid -> List (Html msg)
drawStype level_index scale grid =

    case grid.stype of
        Just a ->
            case a.state of
                Message.SExit _ -> []
                _ ->

                    let
                        pic =
                            case a.content of
                                Chocolate ->
                                    HtmlAttr.src "./assets/chocolate_grid.png"

                                Vanilla ->
                                    HtmlAttr.src "./assets/vanilla_grid.png"
                        (initx,inity) =
                            if level_index==2 then
                                (140,180)
                            else 
                                (0,0)

                    in
                    [ Html.img
                        [ pic
                        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (22.0/(toFloat scale*2.1)) ++ ")")
                        , HtmlAttr.style "position" "absolute"
                        , HtmlAttr.style "left" (String.fromFloat (initx+(toFloat grid.pos.y) * setLength/(toFloat scale)/3.1*64) ++ "px")
                        , HtmlAttr.style "top" (String.fromFloat (inity+(toFloat grid.pos.x) * setLength/(toFloat scale)/3.1*64) ++ "px")
                        ]
                        []
                    ]

        Nothing ->
            []
renderStypes :Int -> Int -> Grids -> List (Html msg)
renderStypes scale level_index grids =
    List.concat (List.map (\y -> Array.toList (Array.map (\x -> drawStype level_index scale x) y)) (Array.toList grids)
        |> List.foldl List.append [])