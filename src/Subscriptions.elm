module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp , onResize)
import Debug exposing (toString)
import Debug exposing (toString)
import Model exposing (Model)
import Message exposing (Msg(..),ArrowKey(..))
import Json.Decode as Decode

import Html.Events exposing (keyCode)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize Resize
        , onKeyDown (Decode.map key keyCode)
        , onKeyUp (Decode.map key keyCode)
        ]

key : Int -> Msg
key keycode =
    case keycode of
        38 ->
            ArrowPressed UpKey
        40 ->
            ArrowPressed DownKey
        37 ->
            ArrowPressed LeftKey
        39 ->
            ArrowPressed RightKey
        32 ->
            ArrowPressed Space
        70 ->
            ArrowPressed F
        _ ->
            ArrowPressed NoKey
            