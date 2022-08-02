module View.Guide exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton,renderTxt)


page1 : Model -> Html Msg
page1 model = 
    let
        opa = (model.move_timer/1000)
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ renderTxt (374,300) 35  "#000000"  "Try to make the first cake!" (Basics.min opa 1)
        , renderTxt (339,400) 35 "#000000"  "Use the arrow keys to control~" (Basics.min (opa-1) 1)
        , renderButton "Go!" LoadNextLevel (560,512) (Basics.min (opa-2) 1) (60,40) "#FFFFFF"

        ]

page2 : Model -> Html Msg
page2 model = 
    let
        opa = (model.move_timer/1000)
    in
    div
        [ HtmlAttr.style "opacity" (String.fromFloat opa)
        ]
        [ renderTxt (250,321) 35  "#000000"  "Control the order to make the second cake!" (Basics.min opa 1)
        , renderButton "Go!" LoadNextLevel (560,462) (Basics.min (model.move_timer/1000-1) 1) (60,40) "#FFFFFF"
        ]

page3 : Model -> Html Msg
page3 model = 
    let
        opa = (model.move_timer/1000)
    in
    div
        [
        ]
        [ renderTxt (250,321) 35  "#000000"  "Create new color by mixing different colors!" (Basics.min opa 1)
        , renderButton "Go!" LoadNextLevel (560,462) (Basics.min (model.move_timer/1000-1) 1) (60,40) "#FFFFFF"
        ]

page4 : Model -> Html Msg
page4 model = 
    let
        opa = (model.move_timer/1000)
    in
    div
        [
        ]
        [ renderTxt (366,321) 35  "#000000"  "Add chocolate on your cake!" (Basics.min opa 1)
        , renderButton "Go!" LoadNextLevel (560,462) (Basics.min (model.move_timer/1000-1) 1) (60,40) "#FFFFFF"
        ]

page5 : Model -> Html Msg
page5 model = 
    let
        opa = (model.move_timer/1000)
    in
    div
        [
        ]
        [ renderTxt (360,300) 35  "#000000"  "You have a new backery now!" (Basics.min opa 1)
        , renderTxt (276,400) 35  "#000000"  "Try to make as much cakes as possible~" (Basics.min (opa-1) 1)
        , renderButton "<" (Load HomePage) (560,512) (Basics.min (opa-2) 1) (60,40) "#FFFFFF"
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

        page =
            case model.level_index of 
                0 -> page1 model
                2 -> page2 model
                4 -> page3 model
                6 -> page4 model
                8 -> page5 model
                _ -> page1 model
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
        [    Html.img
            [ HtmlAttr.src "./assets/gamepage/guide.jpg"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 200 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 133 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.6 ++ ")")
            ][]
            ,page
        ]