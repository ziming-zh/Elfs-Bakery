module View.Cake exposing (Caketype(..), renderCake)

import Color exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)


type Caketype
    = Progress
    | Recipe



--c=[300,]


renderCake : List Color -> Int -> Int -> Float -> Int -> Caketype -> List (Html msg)
renderCake colors x y scale total caketype =
    let
        indexed =
            List.indexedMap Tuple.pair colors
    in
    if total == 0 then
        []

    else
        List.append (List.map (renderith total x y scale) indexed) (renderCandle (List.length colors) total x ((scale / 2) * (0.8 ^ toFloat total)) caketype)


renderCandle : Int -> Int -> Int -> Float -> Caketype -> List (Html msg)
renderCandle now total x scale caketype =
    let
        y =
            case caketype of
                Recipe ->
                    if total == 1 then
                        327

                    else if total == 2 then
                        307

                    else if total == 3 then
                        307

                    else if total == 4 then
                        304

                    else if total == 4 then
                        302

                    else
                        302

                Progress ->
                    if total == 1 then
                        601

                    else if total == 2 then
                        570

                    else if total == 3 then
                        577

                    else if total == 4 then
                        567

                    else if total == 4 then
                        562

                    else
                        562
    in
    if now == total then
        [ Html.img
            [ HtmlAttr.src "./assets/cake/candles.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt (x + 47) ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y) ++ "px")
            ]
            []
        ]

    else
        []


renderith : Int -> Int -> Int -> Float -> ( Int, Color ) -> Html msg
renderith total x y scale ( i, color ) =
    let
        para =
            case total of
                1 ->
                    0.4

                2 ->
                    0.35

                3 ->
                    0.27

                4 ->
                    0.25

                5 ->
                    0.23

                6 ->
                    0.2

                _ ->
                    0.1
    in
    Html.img
        [ selectColor color
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (scale * para * (0.8 ^ toFloat i)) ++ ")")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromInt x ++ "px")
        , HtmlAttr.style "top" (String.fromFloat (toFloat y - (80.0 * scale * para * ((1 - 0.8 ^ toFloat i) / 0.2 + 1))) ++ "px")
        ]
        []


selectColor : Color -> Html.Attribute msg
selectColor color =
    if color == red then
        HtmlAttr.src "./assets/cake/red.png"

    else if color == green then
        HtmlAttr.src "./assets/cake/green.png"

    else if color == orange then
        HtmlAttr.src "./assets/cake/orange.png"

    else if color == lightYellow then
        HtmlAttr.src "./assets/cake/yellow.png"

    else if color == blue then
        HtmlAttr.src "./assets/cake/blue.png"

    else if color == white then
        HtmlAttr.src "./assets/cake/white.png"

    else if color == purple then
        HtmlAttr.src "./assets/cake/purple.png"

    else
        HtmlAttr.src "./assets/cake/grey.png"
