module View.Bgm exposing (gameBGM)
{-| This library contains the bgm

# Function
@docs gameBGM

-}
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html)
import Message exposing (Msg)
{-| render the bgm of the game
-}
gameBGM : Html Msg
gameBGM  = 
        Html.audio
            [ HtmlAttr.src "./assets/bgm.ogg"
            , HtmlAttr.autoplay True
            , HtmlAttr.loop True
            , HtmlAttr.preload "auto"
            , HtmlAttr.style "desplay" "block"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "bottom" "10px"
            , HtmlAttr.controls True
            ]
            []