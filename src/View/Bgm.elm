module View.Bgm exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html)
import Message exposing (Msg)
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