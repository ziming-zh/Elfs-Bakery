module View.Level exposing (..)
import Canvas exposing (Renderable)
import View.Grid exposing (renderGrids)
import Valve exposing (Valve)
import Wall exposing (Wall)
import View.Wall exposing (drawWall)
import View.Valve exposing (renderValves)
import Grid exposing (Grids)
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html,div)
import Message exposing (Msg(..),Page(..))
import Model exposing (Model,GaState(..))
import View.Basic exposing (renderButtonColor)
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
        arraylist = Array.map ( \xx -> Array.toList xx ) model.updatedGrids
        list = List.concat (Array.toList arraylist)
        scalex = toFloat (50*(Tuple.first model.mapSize)+5)
        scaley = toFloat (50*(Tuple.second model.mapSize)+5)
        scale = Basics.max (Tuple.first model.mapSize) (Tuple.second model.mapSize)
        rate = Basics.min (1218/scalex) (790/scaley)
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
            [ HtmlAttr.src "./assets/gamepage/game_interface.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            ][]
            , renderGstate model
        ]
        , renderCake model.color_seq 1439 390 1.2 (List.length model.color_seq) Recipe model.stypes
        , renderCake model.mcolor_seq 1439 750 2.4 (List.length model.color_seq) Progress model.stypes
        , [ toHtml ( round scalex , round scaley)
            [ HtmlAttr.style "transform-origin" "50 50"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat rate ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat (639-(scalex)/2) ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (565-(scaley)/2) ++ "px")]
            (renderLevel model.wall model.valves (model.updatedGrids) model.player)
        ]
        , [renderButtonColor "#4472C4" "<" (Load ChoicePage) (-50,0) 1 (50,50) "#FFFFFF"]
        , View.Grid.renderStypes model.mapSize model.updatedGrids]
       -- , RENDER 
        )
        

renderGstate : Model -> Html Msg
renderGstate model =
    case model.win of
        Model.Playing ->
            renderButtonColor "#4472C4" "Undo" Undo (1380,866) 1 (320,87) "#FFFFFF"
        Model.Lose ->
            renderButtonColor "#4472C4" "Retry" (LoadLevel model.level_index)  (1380,866) 1 (320,87) "#FFFFFF"
        Model.Win ->
            renderButtonColor "#4472C4" "Next Level" (if model.currentPage == GuidePage then LoadNextLevel else (Load ChoicePage)) (1380,866) 1 (320,87) "#FFFFFF"