module Player exposing (..)

import Message exposing (Direction(..), Pos)
import String exposing (left)


type alias Player =
    { pos : Pos
    , dir : Direction
    , state : State
    }


type State
    = Move
    | Stopped


init : Player
init =
    { pos = { x = 0, y = 0 }, dir = Message.Up, state = Stopped }


changeDir : Player -> Direction -> Player
changeDir player dir =
    case dir of
        Stop ->
            player

        _ ->
            { player | dir = dir, state = Move }


move : Player -> Player
move player =
    let
        x =
            player.pos.x

        y =
            player.pos.y
    in
    if player.state == Stopped then
        player

    else
        case player.dir of
            Up ->
                { player | pos = { x = x, y = y - 1 } }

            Right ->
                { player | pos = { x = x + 1, y = y } }

            Down ->
                { player | pos = { x = x, y = y + 1 } }

            Left ->
                { player | pos = { x = x - 1, y = y } }

            Stop ->
                player



