module Update exposing (update)

import Message exposing (Msg(..), stepTime,Direction)
import Model exposing (Model)
import Player
import Wall exposing (Wall, isWall)
import Player exposing (State(..))
import Valve exposing (pushDown,pushLeft,pushUp,pushRight,Valve)
--import Valve exposing(push,isValve)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            ( { model
                | move_timer = model.move_timer + elapsed
              }
                |> timedForward
            , Cmd.none
            )

        ArrowPressed dir ->
            ({ model| player = Player.changeDir model.player dir
            }, Cmd.none
            )

        _ ->
            ( model, Cmd.none )


move : Model -> Model
move model =
    let
        (player,valves) =
            if isWall model.player.pos model.player.dir model.wall == False then
                (Player.move model.player,model.valves)

            --else if isValve model.player model.valves then
                
            --    (Player.move model.player,push model.player model.valves)

            else
                (model.player,model.valves)
    in
    { model | player = { player | state = Stopped }, valves = valves }


timedForward : Model -> Model
timedForward model =
    if model.move_timer > stepTime then
        let
            newModel =
                move model
        in
        { newModel | move_timer = 0 }

    else
        model
pushValve : Player.Model -> List Valve -> List Valve
pushValve player valves =
    let
        x =
            player.pos.x

        y =
            player.pos.y

        dir =
            player.dir

        pos =
            player.pos

        newvalves =
            case dir of
                Message.Up ->
                    pushUp valves pos

                Message.Down ->
                    pushDown valves pos

                Message.Left ->
                    pushLeft valves pos

                Message.Right ->
                    pushRight valves pos

                Message.Stop ->
                    valves
    in
    newvalves