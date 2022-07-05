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
import Html exposing (Html,div)
import Message exposing (Msg(..))
import Model exposing (Model)
import View.Basic exposing (renderTxt,renderButton)

renderLevel : Wall -> List Valve -> Grids -> List Renderable
renderLevel wall valves grids =
    (renderGrids grids) ++
    (drawWall wall) ++
    (renderValves valves) 

renderLevelPage : Model -> Html Msg
renderLevelPage model = 
    let
        ( w , h ) =
            model.windowsize
        level = List.head (model.levels)
        r = 
            if w / h > 1800 / 1000 then
                (h / 1000)

            else
                (w / 1800)
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
        [ Html.img
            [ HtmlAttr.src "./assets/game_interface.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            ][]
        , renderButton "Next" Message.None (1380,866) (320,87) "#FFFFFF"
       -- , renderButton "Guide" Message.None 978 545 "#FFFFFF"
       -- , renderButton "Seting" Message.None 976 655 "#FFFFFF"
        ]