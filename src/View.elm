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
import View.Level exposing (renderLevel,renderLevelPage)
import View.Home exposing(renderHome)
import View.Game exposing(renderGamePage)
import Html.Attributes as HtmlAttr exposing (..)
import Array
import Message exposing (Page(..))

view : Model -> Html Msg
view model =
    let
        ( w , h ) =
            model.windowsize
        level = List.head model.levels
        r = 
            if w / h > 1200 / 800 then
                Basics.min 1 (h / 800)

            else
                Basics.min 1 (w / 1200)
    in

    case model.currentPage of 
        HomePage -> renderHome model
        LevelsPage -> renderLevelPage model
        GamePage -> renderGamePage model
        _ -> 
            div
                [ ]
                [ 
                    -- toHtml (800 ,600)
                    -- []
                    -- (renderLevel model.wall [] (Array.fromList [Array.fromList []]))
                ]

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