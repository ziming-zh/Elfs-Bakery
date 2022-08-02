module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Model exposing (Model)
import Message exposing (Msg(..),key)
import Json.Decode as Decode

import Html.Events exposing (keyCode)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize Resize
        , onKeyDown (Decode.map key keyCode)
        ]
