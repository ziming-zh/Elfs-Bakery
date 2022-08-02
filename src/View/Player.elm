module View.Player exposing (renderPlayer)
import View.Pacman exposing (fromPlayertoFanShape)
import Player exposing (Player)
import Canvas exposing (Renderable)

renderPlayer : Player -> Renderable
renderPlayer player =
    player
        |> fromPlayertoFanShape
        
