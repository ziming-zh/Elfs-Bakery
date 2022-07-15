module View.Choice exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton)



renderChoicePage : Model -> Html Msg
renderChoicePage model =
    let
        ( w , h ) =
            model.windowsize
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
            [ HtmlAttr.src "./assets/game_house.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 72 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 100 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.333 ++ ")")
            ][]
        , renderButton "Level 1" (LoadLevel 1) (880,100) (260,66) "#FFFFFF"
        , renderButton "Level 2" (LoadLevel 1) (880,210) (260,66) "#FFFFFF"
        , renderButton "Level 3" (LoadLevel 1) (880,320) (260,66) "#FFFFFF"
        ]