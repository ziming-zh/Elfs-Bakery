module View.Grid exposing (renderGrids,renderStypes)
{-| This library renders grids and toppings.
-}
import Array
import Canvas exposing (Renderable)
import Color
import Grid exposing (Grid, GridType(..), Grids)
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)
import View.Basic exposing (rectRender, setLength)
import Message exposing (SpecialType(..),Sstate(..))
{-| This function draws grids.
-}
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


drawStype : (Int,Int) -> Grid -> List (Html msg)
drawStype (w,h) grid =
    let
        scalex = toFloat (50*w+5)
        scaley = toFloat (50*h+5)
        rate = Basics.min (1218/scalex) (790/scaley)
    in
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
                        (initx,inity) =(639-21.5+((toFloat grid.pos.y)*setLength-(scalex)/2+2)*rate,565-24.5+((toFloat grid.pos.x)*setLength-(scaley)/2-3)*rate)
                        --(initx,inity) =(639-21.5+((toFloat 0)*setLength-(scalex)/2)*rate,565-24.5+((toFloat 0)*setLength-(scaley)/2)*rate)
                           -- if level_index==1 then
                           --     (40,167)
                           -- else 
                           --     (81,126)

                    in
                    [ Html.img
                        [ pic
                        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (0.55*rate) ++ ")")
                        , HtmlAttr.style "position" "absolute"
                       -- , HtmlAttr.style "left" (String.fromFloat (initx-21.5) ++ "px")
                      --  , HtmlAttr.style "top" (String.fromFloat (inity-24.5) ++ "px")
                        , HtmlAttr.style "left" (String.fromFloat (initx+25*(rate-1)) ++ "px")
                        , HtmlAttr.style "top" (String.fromFloat (inity+25*(rate-1)) ++ "px")
                     --   , HtmlAttr.style "left" (String.fromFloat (initx+(toFloat grid.pos.y) * setLength/(toFloat scale)) ++ "px")
                      --  , HtmlAttr.style "top" (String.fromFloat (inity+(toFloat grid.pos.x) * setLength/(toFloat scale)) ++ "px")
                        ]
                        []
                    ]

        Nothing ->
            []
{-| This function draws toppings on the map.
-}
renderStypes : (Int,Int) -> Grids -> List (Html msg)
renderStypes (w,h) grids =
    List.concat (List.map (\y -> Array.toList (Array.map (\x -> drawStype (w,h) x) y)) (Array.toList grids)
        |> List.foldl List.append [])
