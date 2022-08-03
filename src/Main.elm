module Main exposing (main)
{-| main function

# Function
@docs main

-}
import Browser

--import Levels exposing (Level, ViewLevel)
import Message exposing (Msg(..))
import Model exposing (..)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)



--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    Model.initModel


