module View.Game exposing (..)
import Canvas exposing (Renderable)
import Grid exposing (Grids)
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html,div)
import Message exposing (Msg(..))
import Model exposing (Model)
import View.Basic exposing (renderTxt,renderButton)


renderGamePage : Model -> Html Msg
renderGamePage model = 
    let
        ( w , h ) =
            model.windowsize
        level = List.head (model.levels)
        r = 
            if w / h > 973 / 600 then
                (h / 600)

            else
                (w / 973)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat 973 ++ "px")
        , HtmlAttr.style "height"  (String.fromFloat 600 ++ "px")
        , HtmlAttr.style "left" (String.fromFloat ((w - 973 * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - 600 * r) / 2) ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        ]
        [ Html.img
            [ HtmlAttr.src "./assets/level_1.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            ][]
        , renderButton "Next" Message.None (680,486) (196,52) "#FFFFFF"
        , renderTxt 400 318 68 "#FFFFFF" (String.fromInt model.level_index)
       -- , renderButton "Seting" Message.None 976 655 "#FFFFFF"
        ]