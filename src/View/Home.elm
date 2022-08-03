module View.Home exposing (renderHome)
{-| This library renders the Home Page
-}
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButtonColor)
import Color exposing (Color(..))


{-| This function renders the Home Page
-}
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
            [ HtmlAttr.src "./assets/gamepage/beginning.png"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat -40 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 33 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.1 ++ ")")
            ][]
        , renderButtonColor "#4472C4" "Play" LoadNextLevel (870,400) 1 (250,66) "#FFFFFF"
        , renderButtonColor "#4472C4" "Guide" (Load GuidePage) (870,510) 1 (250,66) "#FFFFFF"
        , renderButtonColor "#4472C4" "Collection" (Load CollectionPage) (870,613) 1 (250,66) "#FFFFFF"
    --    , renderTxt 0 0 10 "#0C0C0B" (String.fromFloat h)
    --    , renderTxt 0 0 10 "#0C0C0B" (String.fromFloat r)
        ]