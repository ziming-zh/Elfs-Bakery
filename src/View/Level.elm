module View.Level exposing (..)
import Canvas exposing (Renderable)
import View.Grid exposing (renderGrids)
import Valve exposing (Valve)
import Wall exposing (Wall)
import View.Wall exposing (drawWall)
import View.Valve exposing (renderValves)
import Grid exposing (Grids,Grid)
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html,div)
import Message exposing (Msg(..),Page(..))
import Model exposing (Model,GaState(..))
import View.Basic exposing(setLength)
import View.Basic exposing (renderButtonColor)
import Player exposing (Player)
import View.Player exposing (renderPlayer)
import Canvas exposing (Renderable,toHtml)
import Array
import View.Hat
import List
import View.Cake exposing (renderCake,Caketype(..))
import View.Cake exposing (renderRecipe,renderRecipeStypes)

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
        ,[ toHtml ( round scalex , round scaley)
            [ HtmlAttr.style "transform-origin" "50 50"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 1472 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 277 ++ "px")]
            (renderRecipe model.color_seq)
        ]
        , renderCake model.color_seq 1439 390 1.2 (List.length model.color_seq) Recipe model.stypes
        , renderCake model.mcolor_seq 1439 750 2.4 (List.length model.color_seq) Progress model.stypes
        , [ toHtml ( round scalex , round scaley)
            [ HtmlAttr.style "transform-origin" "50 50"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat rate ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat (639-(scalex)/2) ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (565-(scaley)/2) ++ "px")]
            ((renderLevel model.wall model.valves (model.updatedGrids) model.player))
        ]
        
        
        , renderRecipeStypes model.stypes
        , [renderButtonColor "#4472C4" "<" (Load ChoicePage) (-50,0) 1 (50,50) "#FFFFFF"]
        , View.Grid.renderStypes model.mapSize model.updatedGrids
        , renderExit model.mapSize model.exit model.level_index model]
       -- , RENDER 
        )

renderExit : (Int,Int) -> Grid -> Int ->Model -> List (Html msg)
renderExit (w,h) grid i model=
    let
        scalex = toFloat (50*w+5)
        scaley = toFloat (50*h+5)
        rate = Basics.min (1218/scalex) (790/scaley)
        pic = 
            case model.currentPage of
                LevelsPage ->
                    View.Hat.hat i
                _ -> View.Hat.hat 1
        (initx,inity) = 
            if (i<=5||model.currentPage==GuidePage) then
                (639-45+((toFloat grid.pos.y)*setLength-(scalex)/2+5)*rate+25*(rate-1),565-24.5+((toFloat grid.pos.x)*setLength-(scaley)/2-3+2)*rate+25*(rate-1))
                        --(initx,inity) =(639-21.5+((toFloat 0)*setLength-(scalex)/2)*rate,565-24.5+((toFloat 0)*setLength-(scaley)/2)*rate)
                           -- if level_index==1 then
                           --     (40,167)
                           -- else 
                           --     (81,126)
            else if i==6 then
                (586,513)
            else
                (787,305)
                    in
                    [ Html.img
                        [ pic
                        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (0.4*rate) ++ ")")
                        , HtmlAttr.style "position" "absolute"
                       -- , HtmlAttr.style "left" (String.fromFloat (initx-21.5) ++ "px")
                      --  , HtmlAttr.style "top" (String.fromFloat (inity-24.5) ++ "px")
                        , HtmlAttr.style "left" (String.fromFloat (initx) ++ "px")
                        , HtmlAttr.style "top" (String.fromFloat (inity) ++ "px")
                     --   , HtmlAttr.style "left" (String.fromFloat (initx+(toFloat grid.pos.y) * setLength/(toFloat scale)) ++ "px")
                      --  , HtmlAttr.style "top" (String.fromFloat (inity+(toFloat grid.pos.x) * setLength/(toFloat scale)) ++ "px")
                        ]
                        []
                    ]


renderGstate : Model -> Html Msg
renderGstate model =
    case model.win of
        Model.Playing ->
            renderButtonColor "#4472C4" "Undo" Undo (1380,866) 1 (320,87) "#FFFFFF"
        Model.Lose ->
            renderButtonColor "#4472C4" "Retry" (LoadLevel model.level_index)  (1380,866) 1 (320,87) "#FFFFFF"
        Model.Win ->
            renderButtonColor "#4472C4" "Next Level" (if model.currentPage == GuidePage then LoadNextLevel else (Load ChoicePage)) (1380,866) 1 (320,87) "#FFFFFF"