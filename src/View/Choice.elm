module View.Choice exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton,renderButtonColor)



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
            [ HtmlAttr.src "./assets/gamepage/level.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 90 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 149 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.6 ++ ")")
            ][]
        , renderButtonColor "#F4B183" "1" (LoadLevel 1) (23,395) 1 (65,65) "#FFFFFF"
        , renderButtonColor "#F4B183" "2" (LoadLevel 2) (184,395) 1 (65,65) "#FFFFFF"
        , renderButtonColor "#F4B183" "3" (LoadLevel 3) (346,395) 1 (65,65) "#FFFFFF"
        , renderButtonColor "#4472C4" "<" (Load HomePage) (-60,0) 1 (50,50) "#FFFFFF"
        --, renderButtonColor "#F4B183" "2" (LoadLevel 2) (100,395) 1 (260,66) "#FFFFFF"
        --, renderButton "Level 3" (LoadLevel 1) (880,320) 1 (260,66) "#FFFFFF"
        ]