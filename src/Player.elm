module Player exposing (init,changeDir,move,Player,State(..))
{-| This library controls the character's movement.
-}
import Message exposing (Direction(..), Pos)

{-| The data type for the player. pos is the current position, dir is the direction it faces, state is whether he is moving or stopped.
-}
type alias Player =
    { pos : Pos
    , dir : Direction
    , state : State
    }

{-| The state of the character. Move means it hasn't moved. Stopped means it has already finished the move.
-}
type State
    = Move
    | Stopped

{-| This function initializes a player.
-}
init : Pos -> Direction -> Player
init initial dirr =
    { pos = initial, dir = dirr, state = Stopped }

{-| This function changes the direction of the player according to the input of the user.
-}
changeDir : Player -> Direction -> Player
changeDir player dir =
    case dir of
        Stop ->
            player

        _ ->
            { player | dir = dir, state = Move }

{-| This function moves the character according to its current direction.
-}
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
