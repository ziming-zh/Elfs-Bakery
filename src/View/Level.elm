module View.Level exposing (..)
import Canvas exposing (Renderable)
import View.Grid exposing (renderGrids)
import Valve exposing (Valve)
import Wall exposing (Wall)
import Grid exposing (Grid)
import View.Wall exposing (drawWall)
import View.Valve exposing (renderValves)
import Grid exposing (Grids)
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html,div,text)
import Message exposing (Msg(..))
import Model exposing (Model)
import View.Basic exposing (renderTxt,renderButton)
import Player exposing (Player)
import View.Player exposing (renderPlayer)
import Canvas exposing (Renderable,toHtml)
import Array
import List
import View.Cake exposing (renderCake,Caketype(..))
renderLevel : Wall -> List Valve -> Grids -> Player -> List Renderable
renderLevel wall valves grids player =
    (renderGrids grids) ++
    (drawWall wall) ++
    (renderValves valves) ++
    [renderPlayer player]


renderLevelPage : Model -> Html Msg
renderLevelPage model = 
    let
        ( w , h ) =
            model.windowsize
        
        r = 
            if w / h > 1800 / 1000 then
                (h / 1000)

            else
                (w / 1800)
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
        [ HtmlAttr.style "width" (String.fromFloat 1800 ++ "px")
        , HtmlAttr.style "height"  (String.fromFloat 1000 ++ "px")
        , HtmlAttr.style "left" (String.fromFloat ((w - 1800 * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - 1000 * r) / 2) ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        ]
        (List.concat
        [
        [ Html.img
            [ HtmlAttr.src "./assets/game_interface.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            ][]
            ,renderButton "Undo" Undo (1380,866) (320,87) "#FFFFFF"
        
        --, renderButton "Next" Message.None (1380,866) (320,87) "#FFFFFF"
        
       -- , renderButton "Guide" Message.None 978 545 "#FFFFFF"
       -- , renderButton "Seting" Message.None 976 655 "#FFFFFF"
        ]
        , renderCake model.color_seq 1439 390 1.2 (List.length model.color_seq) Recipe
        , renderCake model.mcolor_seq 1439 750 2.4 (List.length model.color_seq) Progress
        , [ toHtml (405 ,310)
            [ HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 2.6 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 400 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 420 ++ "px")]
            (renderLevel model.wall model.valves (model.updatedGrids) model.player)
        ], List.map (\xx -> text ((String.fromInt xx)++" ")) dis])

        