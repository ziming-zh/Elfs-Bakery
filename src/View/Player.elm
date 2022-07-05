module View.Player exposing (..)
import Html exposing (Html)
import Message exposing (Msg)
import View.Pacman exposing (fromPlayertoFanShape)
import Player exposing (Player)
import Canvas exposing (Renderable)

renderPlayer : Player -> Renderable
renderPlayer player =
    player
        |> fromPlayertoFanShape
        
