{- 
Valve is the control gadgets that deals with the flows of the stream
-}

module Valve exposing (..)
import Message exposing (Pos)
import Message exposing (MoveDirection(..))

type alias Valve = 
    { state : VState
    , pos : Pos
    }

type VState 
    = UpRight
    | UpLeft
    | DownLeft
    | DownRight


{-
isValve: {pos:Pos,dir:MoveDirection}->List Valve -> Bool
isValve {pos,dir} valves =
    let 
        x=pos.x
        y=pos.y
    in

        case dir of
            Right -> 
                List.any (isEqual valves
-}
isEqual: Valve->Pos-> Bool
isEqual valve pos =
    valve.pos==pos
{-
pushValve: {pos:Pos,dir:MoveDirection}->List Valve -> Valve
pushValve {pos,dir} valves =
    case dir of
        Right ->
        -}