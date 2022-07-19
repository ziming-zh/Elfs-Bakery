module View.Collection exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing(renderButtonColor)
import Svg.Attributes exposing (mode)

check : List Bool -> Int -> Bool
check list k =
    case List.head (List.drop (k-1) list) of
        Just a -> a
        Nothing -> False

get : List Float -> Int -> Float
get list k =
    case List.head (List.drop (k-1) list) of
        Just a -> a
        Nothing -> 0

pageSingleRank : List Bool -> Int -> Html Msg
pageSingleRank cleared k =
    let
        list = [250,433.3,616.6,800,341.67,525,708.3]
        listx = List.map (\xx -> xx-20) list
        listy = [320,320,320,320,500,500,500]
        (x,y) = (get listx k,get listy k)
    in
        Html.img
            [ HtmlAttr.src ("./assets/rank/rank" ++ ( if check cleared k then (String.fromInt k)  else "unknown" ) ++ ".png" )
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.5 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat y ++ "px")
            ][]

pageHat : Model -> Html Msg
pageHat model =
    let
        opa = model.move_timer/1000
    in
    div 
        [ style "opacity" (String.fromFloat (Basics.min 1 opa))
        ]
        (List.map (pageSingleRank model.level_cleared) (List.range 0 7))

pageMedal : Model -> Html Msg
pageMedal model =
    div [][]

pageNone : Html Msg
pageNone =
    div [][]

renderCollectionPage : Model -> Html Msg
renderCollectionPage model =
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
            [ HtmlAttr.src "./assets/gamepage/collection.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 0.94 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 50 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 15 ++ "px")
            ][]
        , renderButtonColor "#FFC000" "Medal" (LoadLevel 1) (1000,0) 1 (200,50) "#FFFFFF"
        , renderButtonColor "#FFC000" "Hat" (LoadLevel 2) (1000,50) 1 (200,50) "#FFFFFF"
        , renderButtonColor "#4472C4" "<" (Load HomePage) (-60,0) 1 (50,50) "#FFFFFF"
        , (case model.level_index of
            0 -> pageNone
            1 -> pageMedal model
            2 -> pageHat model
            _ -> pageNone
          )
        ]