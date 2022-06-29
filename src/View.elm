module View exposing (view)
import Model exposing (Model)
import Message exposing (Msg(..))
import Html exposing (Html, button, div, text)
import Canvas exposing (toHtml)
import View.Wall exposing (drawWall)
import Model exposing (Mapset)
import View.Basic exposing (rectRender)
import Color exposing (Color)
import Canvas exposing (clear)
import Canvas exposing (rect)
import Canvas exposing (shapes)
import Html.Attributes exposing (style)
import Canvas.Settings exposing (fill)

view : Model -> Html Msg
view model =
    let
        wall = model.wall
        
    in
    div
        [ ]
        [ toHtml (800 ,600)
            []
            (drawWall wall)
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