module Main exposing (main)

import Browser




import Model exposing (Model)
import Message exposing (Msg(..))
import Update exposing (update)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Task
import Browser
import Model exposing (..)
import Levels exposing (Level,ViewLevel)
import LevelSeq exposing (LevelSeq)
--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

init :  ( Model, Cmd Msg )
init  =
    Model.initModel



{-
    ( model_init , Task.perform GetViewport getViewport )

    Browser.element
        { init =
            \value ->
                ( value
                    |> Decode.decodeValue Model.decode
                    |> Result.withDefault Model.initial
                , Task.perform GetViewport getViewport
                )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }
-}