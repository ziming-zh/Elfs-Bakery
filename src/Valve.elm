{- 
Valve is the control gadgets that deals with the flows of the stream
-}

module Valve exposing (..)

type alias Valve = 
    { state : (VState, VState)
    , pos : (Int, Int)
    }

type VState 
    = UpRight
    | UpLeft
    | DownLeft
    | DownRight
