{- 
Valve is the control gadgets that deals with the flows of the stream
-}

module Valve exposing (..)
import Message exposing (Pos)
import Message exposing (Direction(..))

type alias Valve = 
    { state : VState
    , pos : Pos
    }

type VState 
    = Up
    | Down
    | Left
    | Right

initValve : Int -> Int -> VState -> Valve 
initValve x y state =
    {state = state, pos = {x=x, y=y}}

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