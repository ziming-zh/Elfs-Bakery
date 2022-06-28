module Main exposing (main)

import Browser
import LevelSeq exposing (LevelSeq)
--import Levels exposing (Level, ViewLevel)
import Message exposing (Msg(..), key)
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
