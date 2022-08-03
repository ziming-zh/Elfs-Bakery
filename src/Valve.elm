module Valve exposing (clockRotate,counterRotate,pushDown,pushRight,pushLeft,pushUp,VState(..), Valve)
{-| Valve is the control gadgets that deals with the flows of the stream

# Functions
@docs clockRotate,counterRotate,pushDown,pushRight,pushLeft,pushUp

# Data Types
@docs VState, Valve

-}
import Message exposing (Direction(..), Pos)

{-| Valve is the control gadgets that deals with the flows of the stream. 
pos is the position of the origin and state tells it relative position wrt the origin.
-}
type alias Valve =
    { state : VState
    , pos : Pos
    }


{-| the direction of the valve with respect to it origin.
-}
type VState
    = Up
    | Down
    | Left
    | Right




{-| This function rotates a valve clockwise.
-}
clockRotate : Valve -> Valve
clockRotate valve =
    case valve.state of
        Up ->
            { valve | state = Right }

        Right ->
            { valve | state = Down }

        Down ->
            { valve | state = Left }

        Left ->
            { valve | state = Up }

{-| This function rotates a valve counterclockwise.
-}
counterRotate : Valve -> Valve
counterRotate valve =
    case valve.state of
        Up ->
            { valve | state = Left }

        Right ->
            { valve | state = Up }

        Down ->
            { valve | state = Right }

        Left ->
            { valve | state = Down }

{-| This function pushes a level valve up.
-}
pushUp : List Valve -> Pos -> List Valve
pushUp valves pos =
    let
        x =
            pos.x

        y =
            pos.y

        ( leftUp, leftall ) =
            List.partition (\valve -> valve.pos.x == x && valve.pos.y == y) valves

        ( rightUp, left ) =
            List.partition (\valve -> valve.pos.x == x && valve.pos.y == (y + 1)) leftall

        lu =
            if List.any (\valve -> valve.state == Right) leftUp then
                List.map counterRotate leftUp

            else
                leftUp

        ru =
            if List.any (\valve -> valve.state == Left) rightUp then
                List.map clockRotate rightUp

            else
                rightUp
    in
    List.append lu left
        |> List.append ru

{-| This function pushes a vertical valve right.
-}
pushRight : List Valve -> Pos -> List Valve
pushRight valves pos =
    let
        x =
            pos.x

        y =
            pos.y

        ( rightUp, leftall ) =
            List.partition (\valve -> valve.pos.x == x && valve.pos.y == (y + 1)) valves

        ( rightDown, left ) =
            List.partition (\valve -> valve.pos.x == (x + 1) && valve.pos.y == (y + 1)) leftall

        rd =
            if List.any (\valve -> valve.state == Up) rightDown then
                List.map clockRotate rightDown

            else
                rightDown

        ru =
            if List.any (\valve -> valve.state == Down) rightUp then
                List.map counterRotate rightUp

            else
                rightUp
    in
    List.append rd left
        |> List.append ru

{-| This function pushes a vertical valve left.
-}
pushLeft : List Valve -> Pos -> List Valve
pushLeft valves pos =
    let
        x =
            pos.x

        y =
            pos.y

        ( leftUp, leftall ) =
            List.partition (\valve -> valve.pos.x == x && valve.pos.y == y) valves

        ( leftDown, left ) =
            List.partition (\valve -> valve.pos.x == (x + 1) && valve.pos.y == y) leftall

        ld =
            if List.any (\valve -> valve.state == Up) leftDown then
                List.map counterRotate leftDown

            else
                leftDown

        lu =
            if List.any (\valve -> valve.state == Down) leftUp then
                List.map clockRotate leftUp

            else
                leftUp
    in
    List.append ld left
        |> List.append lu

{-| This function pushes a level valve down.
-}
pushDown : List Valve -> Pos -> List Valve
pushDown valves pos =
    let
        x =
            pos.x

        y =
            pos.y

        ( leftDown, leftall ) =
            List.partition (\valve -> valve.pos.x == (x + 1) && valve.pos.y == y) valves

        ( rightDown, left ) =
            List.partition (\valve -> valve.pos.x == (x + 1) && valve.pos.y == (y + 1)) leftall

        ld =
            if List.any (\valve -> valve.state == Right) leftDown then
                List.map clockRotate leftDown

            else
                leftDown

        rd =
            if List.any (\valve -> valve.state == Left) rightDown then
                List.map counterRotate rightDown

            else
                rightDown
    in
    List.append ld left
        |> List.append rd
