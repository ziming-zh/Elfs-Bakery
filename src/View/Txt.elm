module View.Txt exposing (renderTxt)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Color exposing (Color)
import Message exposing (Msg(..))


renderTxt : Int -> Int -> Int -> String -> String -> Html Msg
renderTxt x y size color txt = 
    div
        [ style "left" (String.fromInt x ++ "px")
        , style "top" (String.fromInt y ++ "px")
        , style "font_size" (String.fromInt size ++ "px")
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "color" color
        ]
        [ text txt ]