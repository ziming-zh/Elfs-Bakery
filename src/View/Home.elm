module View.Home exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, button, div, text)
import View.Basic exposing (renderTxt,renderButton)
import Color exposing (Color(..))




renderHome : Model -> Html Msg
renderHome model = 
    let
        ( w , h ) =
            model.windowsize
        level = List.head (model.levels)
        r = 
            if w / h > 1200 / 800 then
                (h / 800)

            else
                Basics.min 1 (w / 1200)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat 1200 ++ "px")
        , HtmlAttr.style "height"  (String.fromFloat 800 ++ "px")
        , HtmlAttr.style "left" (String.fromFloat ((w - 1200 * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - 800 * r) / 2) ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        ]
        [ Html.img
            [ HtmlAttr.src "./assets/game_beginning_1.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 100 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.333 ++ ")")
            ][]
        , renderButton "Play" LoadNextLevel (880,425) (260,66) "#FFFFFF"
        , renderButton "Guide" Message.None (880,535) (260,66) "#FFFFFF"
        , renderButton "Seting" Message.None (880,645) (260,66) "#FFFFFF"
    --    , renderTxt 0 0 10 "#0C0C0B" (String.fromFloat h)
    --    , renderTxt 0 0 10 "#0C0C0B" (String.fromFloat r)
        ]