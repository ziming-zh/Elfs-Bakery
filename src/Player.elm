module Player exposing (..)

import Message exposing (Direction(..), Pos)


type alias Player =
    { pos : Pos
    , dir : Direction
    , state : State
    }


type State
    = Move
    | Stopped


init : Pos -> Direction -> Player
init initial dirr =
    { pos = initial, dir = dirr, state = Stopped }


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
                { player | pos = { x = (x - 1), y = y } }

            Right ->
                { player | pos = { x = x, y = y + 1 } }

            Down ->
                { player | pos = { x = x + 1, y = y } }

            Left ->
                { player | pos = { x = x, y = (y - 1) } }

            Stop ->
                player
