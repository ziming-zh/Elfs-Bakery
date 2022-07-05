module Player exposing (..)

import Message exposing (Direction(..), Pos)


type alias Model =
    { pos : Pos
    , dir : Direction
    , state : State
    }

type State
    = Move
    | Stopped

init : Model
init =
    { pos = { x = 1, y = 1 }, dir = Up,state = Stopped }

changeDir: Model->Direction -> Model
changeDir model dir =
    case dir of
        Stop -> model
        _ -> {model|dir=dir,state=Move}
        


move: Model -> Model
move model =
    let
        x=model.pos.x
        y=model.pos.y
    in
    if model.state == Stopped then
        model
    else 
        case model.dir of
            Up -> 
                {model|pos={x=x,y=y-1}}
            Right ->
                {model|pos={x=x+1,y=y}}
            Down -> 
                {model|pos={x=x,y=y+1}}
            Left ->
                {model|pos={x=x-1,y=y}}
            Stop ->
                model
                
            
