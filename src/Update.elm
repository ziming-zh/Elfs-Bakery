module Update exposing (update)
import Array
import Message exposing (Msg(..), stepTime,Direction)
import Model exposing (Model,updateGridsfromModel)
import Wall exposing (Wall, isWall)
import Player exposing (State(..),Player)
import Valve exposing (pushDown,pushLeft,pushUp,pushRight,Valve)
import Grid exposing (getGstate,sendPainttoGrids,getGrid)
import Grid exposing (IsOpen)
import Grid exposing (IsOpen(..),Grids,movePaint)
import DBFS exposing (bfs)
import Message exposing (Paint)
import Color
import Model exposing (loadValves)
import Grid exposing (getDistance)
-- import Grid exposing (clearPaintGrid)
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
            case getGstate model.player.pos model.updatedGrids model.player.dir of 
                Open ->
                    (Player.move model.player,model.valves)
                FakeClose ->
                    (Player.move model.player,pushValve model.player model.valves )
                Close ->
                    (model.player,model.valves)
        
        newmodel={ model | player = { player | state = Stopped }, valves = valves }
        newgrids=loadValves newmodel.grids newmodel.valves


        newnewmodel={newmodel|paints=movePaints newmodel newgrids model.paints}
    in
        {newnewmodel|updatedGrids = (updateGridsfromModel newnewmodel newmodel.grids)|>bfs model.exit  }
movePaintsRecur : Model -> Grids  -> List Paint -> Int -> List Paint
movePaintsRecur model grids paints i =
    let
        l=List.length paints
        arrPaints=Array.fromList paints
    in
        if i<l then
            let 
                defaultPaint= {pos = {x=-1,y=-1},color=Color.lightYellow}
                paintMoving =Maybe.withDefault defaultPaint (Array.get i arrPaints)
                newPaints = Array.toList (Array.set i (Grid.movePaint grids paintMoving) arrPaints)
                newGrid = bfs model.exit (loadValves model.grids model.valves)
                newnewGrid = (List.foldl sendPainttoGrids newGrid newPaints)
            in
                movePaintsRecur model newnewGrid newPaints (i+1)
        else 
            paints
        
movePaints : Model -> Grids -> List Paint -> List Paint
movePaints model grids paints =
    let
        sorted = List.sortBy  (\x -> getDistance x.pos grids) paints
    in
        movePaintsRecur model grids sorted 0
    

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
pushValve : Player -> List Valve -> List Valve
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