{-
   Valve is the control gadgets that deals with the flows of the stream
-}


module Valve exposing (..)

import Message exposing (Direction(..), Pos)


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
    { state = state, pos = { x = x, y = y } }



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


isEqual : Valve -> Pos -> Bool
isEqual valve pos =
    valve.pos == pos



{-
   pushValve: {pos:Pos,dir:MoveDirection}->List Valve -> Valve
   pushValve {pos,dir} valves =
       case dir of
           Right ->


        pushValve: {pos:Pos,dir:MoveDirection}->List Valve -> Valve
   pushValve {pos,dir} valves =
       case dir of
           Right ->

push
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





updateRotate : Maybe Valve -> List Valve -> List Valve
updateRotate valve valves =
    case valve of
        Nothing ->
            valves

        Just v ->
            v :: valves


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
            let
                valvelu =
                    List.head leftUp

                updatedlu =
                    case valvelu of
                        Nothing ->
                            valvelu

                        Just v ->
                            case v.state of
                                Right ->
                                    Just (counterRotate v)

                                _ ->
                                    Just v
            in
            updatedlu

        ru =
            let
                valveru =
                    List.head rightUp

                updatedru =
                    case valveru of
                        Nothing ->
                            valveru

                        Just v ->
                            case v.state of
                                Left ->
                                    Just (clockRotate v)

                                _ ->
                                    Just v
            in
            updatedru

        newValves =
            updateRotate lu left
                |> updateRotate ru
    in
    newValves


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

        ru =
            let
                valveru =
                    List.head rightUp

                updatedru =
                    case valveru of
                        Nothing ->
                            valveru

                        Just v ->
                            case v.state of
                                Down ->
                                    Just (counterRotate v)

                                _ ->
                                    Just v
            in
            updatedru

        rd =
            let
                valverd =
                    List.head rightDown

                updatedrd =
                    case valverd of
                        Nothing ->
                            valverd

                        Just v ->
                            case v.state of
                                Up ->
                                    Just (clockRotate v)

                                _ ->
                                    Just v
            in
            updatedrd

        newValves =
            updateRotate rd left
                |> updateRotate ru
    in
    newValves


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

        lu =
            let
                valvelu =
                    List.head leftUp

                updatedlu =
                    case valvelu of
                        Nothing ->
                            valvelu

                        Just v ->
                            case v.state of
                                Down ->
                                    Just (clockRotate v)

                                _ ->
                                    Just v
            in
            updatedlu

        ld =
            let
                valveld =
                    List.head leftDown

                updatedld =
                    case valveld of
                        Nothing ->
                            valveld

                        Just v ->
                            case v.state of
                                Up ->
                                    Just (counterRotate v)

                                _ ->
                                    Just v
            in
            updatedld

        newValves =
            updateRotate lu left
                |> updateRotate ld
    in
    newValves


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
            let
                valveld =
                    List.head leftDown

                updatedld =
                    case valveld of
                        Nothing ->
                            valveld

                        Just v ->
                            case v.state of
                                Right ->
                                    Just (clockRotate v)

                                _ ->
                                    Just v
            in
            updatedld

        rd =
            let
                valverd =
                    List.head rightDown

                updatedrd =
                    case valverd of
                        Nothing ->
                            valverd

                        Just v ->
                            case v.state of
                                Left ->
                                    Just (counterRotate v)

                                _ ->
                                    Just v
            in
            updatedrd

        newValves =
            updateRotate rd left
                |> updateRotate ld
    in
    newValves
