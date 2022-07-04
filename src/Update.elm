module Update exposing (update)

import Message exposing (Msg(..), stepTime,Direction)
import Model exposing (Model,updateGridsfromModel)
import Player
import Wall exposing (Wall, isWall)
import Player exposing (State(..))
import Valve exposing (pushDown,pushLeft,pushUp,pushRight,Valve)
import Grid exposing (getGstate)
import Grid exposing (IsOpen)
import Grid exposing (IsOpen(..),Grids,movePaint)
import DBFS exposing (bfs)
import Message exposing (Paint)
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
            case getGstate model.player.pos model.grids model.player.dir of 
                Open ->
                    (Player.move model.player,model.valves)
                FakeClose ->
                    (model.player,pushValve model.player model.valves )
                Close ->
                    (model.player,model.valves)
        newmodel={ model | player = { player | state = Stopped }, valves = valves ,paints=List.map (movePaint model.grids) model.paints}
    in
    { newmodel | grids
        =updateGridsfromModel newmodel newmodel.grids
        |> bfs model.exit
    }



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