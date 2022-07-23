module View.Cake exposing (Caketype(..), renderCake,renderRecipeDeco,renderProgressDeco)

import Color exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)
import Message exposing (SpecialType(..), Stype)
import Model exposing (Model)

type Caketype
    = Progress
    | Recipe
    | Task



--c=[300,]


selectDeco : Stype -> Html.Attribute msg
selectDeco stype =
    case stype.content of
        Chocolate ->
            HtmlAttr.src "./assets/chocolate.png"

        Vanilla ->
            HtmlAttr.src "./assets/vanilla.png"

renderProgressDeco : Int -> Stype -> List (Html msg)
renderProgressDeco total stype =
        case stype.state of
            Message.SExit i -> 
                [renderith total 1487 785 5.6 Cream i  (selectDeco stype)]
            _ -> []
renderRecipeDeco: Int -> Stype ->Html msg
renderRecipeDeco total stype =
    renderith total 1487 424 2.8 Cream stype.target  (selectDeco stype)
renderCake : List Color -> Int -> Int -> Float -> Int -> Caketype -> List (Html msg)
renderCake colors x y scale total caketype =
    let
        indexed =
            List.indexedMap Tuple.pair colors
        index=List.range 0 (List.length colors)
    in
    if total == 0 then
        []

    else
        List.append (List.map2 (renderith total x y scale Cake) index (List.map selectColor colors)) (renderCandle (List.length colors) total x ((scale / 2) * (0.8 ^ toFloat total)) caketype)


renderCandle : Int -> Int -> Int -> Float -> Caketype -> List (Html msg)
renderCandle now total x scale caketype =
    let
        yo =
            if scale == 1.2 then
                        if total == 1 then
                            327

                        else if total == 2 then
                            307

                        else if total == 3 then
                            307

                        else if total == 4 then
                            304

                        else if total == 5 then
                            302

                        else if total ==6 then
                            312
                        else
                            302
            else
                        if total == 1 then
                            601

                        else if total == 2 then
                            570

                        else if total == 3 then
                            577

                        else if total == 4 then
                            567

                        else if total == 5 then
                            562
                        else if total ==6 then
                            575
                        else
                            562
        y = 
            case caketype of
                Recipe -> yo
                Progress -> yo
                Task -> (yo-110)
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

type Ithtype =Cream|Cake
renderith : Int -> Int -> Int -> Float -> Ithtype-> Int  -> Html.Attribute msg -> Html msg
renderith total x y scale itemtype i item =
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
    case itemtype of 
        Cake ->
            Html.img
            [ item
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (scale * para * (0.8 ^ toFloat i)) ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y - (80.0 * scale * para * ((1 - 0.8 ^ toFloat i) / 0.2 + 1))) ++ "px")
            ]
            []
        Cream ->
            Html.img
            [ item
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (scale * para * (0.8 ^ toFloat i)) ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y - (80.0 * scale/2.8*1.2 * para * ((1 - 0.8 ^ toFloat i) / 0.2 + 1))) ++ "px")
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
