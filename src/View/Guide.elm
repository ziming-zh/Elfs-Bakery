module View.Guide exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton,renderImg)


page1 : Model -> Html Msg
page1 model = 
    let
        opa = Basics.min (model.move_timer/1000) 1
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ renderImg "./assets/font/1.png" (1,100)
        , renderButton "Go!" LoadNextLevel (150,200) (Basics.min (model.move_timer/1000-1) 1) (50,50) "#FFFFFF"
        ]

page2 : Model -> Html Msg
page2 model = 
    let
        opa = Basics.min (model.move_timer/1000) 1
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ renderImg "./assets/font/2.png" (1,100)
        , renderButton "Go!" LoadNextLevel (300,200) (Basics.min (model.move_timer/1000-1) 1) (50,50) "#FFFFFF"
        ]

page3 : Model -> Html Msg
page3 model = 
    let
        opa = Basics.min (model.move_timer/1000) 1
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ renderImg "./assets/font/3.png" (1,100)
        , renderButton "Go!" LoadNextLevel (300,200) (Basics.min (model.move_timer/1000-1) 1) (50,50) "#FFFFFF"
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
        [ case model.level_index of 
            0 -> page1 model
            2 -> page2 model
            4 -> page3 model
            _ -> page1 model
        ]