module View.Guide exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton)


page2 : Model -> Html Msg

page1 : Model -> Html Msg
page1 model = 
    let
        opa = Basics.min (model.move_timer/1000) 1
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ Html.img
            [ HtmlAttr.src "./assets/font/1.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 100 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.333 ++ ")")
            ][] 
        , renderButton "Go!" LoadNextLevel (150,200) (Basics.min (model.move_timer/1000-1) 1) (50,50) "#FFFFFF"
        ]

renderGuidePage : Model -> Html Msg
renderGuidePage model =
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
        [ page1 model
        ]