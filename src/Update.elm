module Update exposing (update)
{-| This library combines all the update functions.

# Functions
@docs update

-}
import Array
import Message exposing (Msg(..), stepTime,Direction)
import Model exposing (Model,getModel)
import Wall exposing (Wall)
import Player exposing (State(..),Player)
import Valve exposing (pushDown,pushLeft,pushUp,pushRight,Valve)
import Grid exposing (getGstate,sendPainttoGrids,sendStype2Grid,loadValve)
import Grid exposing (IsOpen(..),Grids,movePaint)
import DBFS exposing (bfs)
import Message exposing (Paint)
import Color
import Model exposing (getModel,GaState(..))
import Grid exposing (getDistance)
import Player exposing (State(..))
import Message exposing (Page(..))
import Message exposing (Pos)
import Html exposing (a)
import Task
import Grid exposing (updateSpecialType)
{-| The update function for the model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            if model.win == Model.Playing then
                ( { model
                    | move_timer = model.move_timer + elapsed
                }
                    |> timedForward elapsed
                    |> checkEnd
                , Cmd.none
                )
            else if model.currentPage == GuidePage || model.currentPage == CollectionPage then
                ( { model | move_timer = model.move_timer + elapsed } , Cmd.none )
            else (model,Cmd.none)

        ArrowPressed dir ->
            if model.win == Model.Playing then
                let
                    level = model.level
                    nlevel={level|player = Player.changeDir model.level.player dir}
                in
                ({ model| level=nlevel}, Cmd.none
                )
            else (model,Cmd.none)

        GetViewport { viewport } ->
            ( { model
                | windowsize =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none ) 

        Resize width height ->
            ( { model | windowsize = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        LoadNextLevel ->
            case model.currentPage of
                GuidePage ->
                    if Basics.modBy 2 model.level_index == 0  then
                        let
                            index = model.level_index + 1
                            (nmodel,nmsg) = getModel (round ((toFloat (model.level_index+1))/2)) model 
                        in
                            ( { nmodel | currentPage = GuidePage , level_index = index } , nmsg )
                        
                    else
                        ( { model | level_index = model.level_index + 1 , move_timer = 0 } , Cmd.none )
                HomePage -> 
                    ( { model | currentPage = ChoicePage , level_index = 0 } , Cmd.none )
                _ -> 
                    ( model , Cmd.none )
        Undo ->
                case List.head model.history of
                    Nothing -> (model,Cmd.none)
                    Just h -> 
                        let
                            hplayer=h.player
                            level=model.level
                            nlevel={level|valves=h.valves,player={hplayer|state=Player.Stopped},paints=h.paints,stypes=h.stypes}
                            nmodel = {model|level=nlevel}
                            
                            exit = Grid.initGrid model.level.exit.x model.level.exit.y
                        in
                        ({nmodel|updatedGrids=(updateGridsfromModel nmodel model.grids)|> bfs exit,history =List.drop 1 model.history},Cmd.none)


        LoadLevel k ->
            case model.currentPage of
                CollectionPage -> ( { model | level_index = k , move_timer = 0 } , Cmd.none )
                GuidePage -> 
                    let
                        (nmodel,nmsg) = getModel (round ((toFloat (model.level_index+1))/2)) model 
                    in  
                        ( { nmodel | currentPage = GuidePage , level_index = model.level_index } , nmsg )
                _ ->
                    getModel k model

        Load page ->
            ( { model | currentPage = page , move_timer = 0 , level_index = 0 
                        , win = (if page == CollectionPage then Lose else model.win ) } , Cmd.none )
        ChoiceInfo i-> 
            ({ model | level_index = i },Cmd.none)
        _ ->
            ( model, Cmd.none )

checkEnd : Model -> (Model)
checkEnd model =
    let 
        k = model.level_index
        list = model.level_cleared
    in
        if model.mcolor_seq==model.level.colorseq &&checkEqualStypes model then
            {model|win = Model.Win,level_cleared = List.concat[List.take (k-1) list,[True],List.drop k list]}
        else if compareList model.mcolor_seq model.level.colorseq && checkGameStypes model then
            model
        else
            {model|win= Model.Lose}

checkGameStype : Int -> Message.Stype -> Bool
checkGameStype now stype =
    case stype.state of
        Message.SExit i ->
            if i /= stype.target then
                False
            else 
                True
        _ ->
            if stype.target>=now then
                True
            else
                False
checkGameStypes : Model -> Bool
checkGameStypes model =
    let
        now = List.length model.mcolor_seq
        stypes=model.level.stypes
    in
        List.all isTrue (List.map (checkGameStype now) stypes)
isTrue : Bool -> Bool
isTrue bool =
    bool
checkEqualStypes : Model -> Bool
checkEqualStypes model =
    let
        now = List.length model.level.colorseq
        stypes=model.level.stypes
    in
        List.all isTrue (List.map checkEqualStype  stypes)
checkEqualStype : Message.Stype -> Bool
checkEqualStype stype =
    case stype.state of
        Message.SExit i ->
            if stype.target == i then
                True
            else 
                False
        _ -> False


checkSpecialExit : Model -> Message.Stype -> Message.Stype 
checkSpecialExit model stype =
    let
        exit=model.level.exit
        now =List.length model.mcolor_seq
        nstype=
            if exit == stype.pos then
                case stype.state of 
                    Message.Moving ->
                        {stype|state=Message.SExit (now-1)}
                    _ -> stype
            else
                stype
        in 
            nstype
checkSpecialExits : Model -> Model
checkSpecialExits model = 
    let 
        stypes=model.level.stypes
    
        nstypes=List.map (checkSpecialExit model) model.level.stypes
        level=model.level
        nlevel={level|stypes=nstypes}
    in 
        {model|level=nlevel}
compareList : List a -> List a -> Bool
compareList a b =
    let 
        al=List.length a
        bl=List.length b
        min=Basics.min al bl
        aa=List.take min a
        ba=List.take min b
    in 
        aa==ba
move : Model -> Model
move model =
    let
        (player,valves) =
            case getGstate model.level.player.pos model.updatedGrids model.level.player.dir of 
                Open ->
                    (Player.move model.level.player,model.level.valves)
                FakeClose ->
                    (Player.move model.level.player,pushValve model.level.player model.level.valves )
                Close ->
                    (model.level.player,model.level.valves)
        n_modelhis =
            if valves == model.level.valves then
                model
            else
                {model|history ={paints=model.level.paints,valves=model.level.valves,player=model.level.player,stypes=model.level.stypes} ::model.history}
        level=n_modelhis.level
        nlevel={level| player = { player | state = Stopped }, valves = valves}
        newmodel={ n_modelhis | level=nlevel}
        newgrids=loadValves newmodel.grids newmodel.level.valves
        nmodel={newmodel|updatedGrids=newgrids}
        exit = Grid.initGrid model.level.exit.x model.level.exit.y
        
    in
        {nmodel|updatedGrids = updateGridsfromModel nmodel nmodel.grids|>bfs exit  }


posequal : Pos -> Pos -> Bool
posequal pos1 pos2 =
    pos1.x == pos2.x && pos1.y == pos2.y

movePaintsRecur : (Model, List Paint) -> Grids -> Int -> (Model,List Paint)
movePaintsRecur (model,paints) grids  i =
    let
        l=List.length paints
        arrPaints=Array.fromList paints
        exit=Grid.initGrid model.level.exit.x model.level.exit.y
    in
        if i<l then
            let 
                newPaints = Array.toList ( Grid.movePaint grids i arrPaints )
                newGrid = bfs exit (loadValves model.grids model.level.valves)
                newnewGrid = (List.foldl sendPainttoGrids newGrid newPaints)
            in
                movePaintsRecur (model, newPaints) newnewGrid  (i+1)
        else 
            let
                exitpaint=Tuple.first (List.partition (\x -> (posequal x.pos model.level.exit)) paints)
                normalpaint = Tuple.second (List.partition (\x -> (posequal x.pos model.level.exit)) paints)
                lastpaint = List.head ( List.drop ((List.length model.mcolor_seq)-1) model.mcolor_seq )
                (thispaint,history) = 
                    case List.head exitpaint of
                        Just a -> (a,[])
                        Nothing ->  ({ pos = Pos 0 0 , color = Color.red },model.history)
                mcolorseq = 
                    case lastpaint of
                        Just a ->
                            if a == thispaint.color then
                                model.mcolor_seq
                            else 
                                model.mcolor_seq ++ List.map (\x -> x.color) exitpaint
                        Nothing -> 
                            model.mcolor_seq ++ List.map (\x -> x.color) exitpaint
            in
                ({model|mcolor_seq = mcolorseq,history=history}, List.sortBy (\x -> getDistance x.pos grids) normalpaint)
        
movePaints : (Model, List Paint) -> Grids ->(Model, List Paint)
movePaints (model,paints) grids  =
    let
        sorted = List.sortBy  (\x -> getDistance x.pos grids) paints
    in
        movePaintsRecur (model,sorted) grids  0
    


timedForward : Float -> Model -> Model
timedForward elapsed model =
    if ( model.currentPage == GuidePage && ( Basics.modBy 2 model.level_index == 0 ) )
      || model.currentPage == CollectionPage then model
        else
    if model.move_timer > stepTime then
        let
            exit = Grid.initGrid model.level.exit.x model.level.exit.y
            newModel =
                move model
            movedPaint =
                    let 
                        ngrids=(newModel.updatedGrids |> bfs exit)
                        nstypes=List.map (Grid.updateSpecialType ngrids) model.level.stypes
                        (nmodel,npaints) = movePaints (newModel, newModel.level.paints) (newModel.updatedGrids |> bfs exit)
                        level=model.level
                        nlevel={level|paints=npaints,stypes=List.map (Grid.moveSpecialType  ngrids) nstypes}
                        newnewmodel={nmodel|level=nlevel}
                        finalmodel = 
                            if nmodel.mcolor_seq /= newModel.mcolor_seq then
                                checkSpecialExits newnewmodel
                            else 
                                let 
                                    newnewlevel=
                                        { nlevel | stypes = List.filter ( \x -> ( x.pos /= exit.pos || x.state /= Message.Moving ) ) newnewmodel.level.stypes  }
                                        
                                in
                                    {newnewmodel|level=nlevel}
                    in
                        {finalmodel|updatedGrids = updateGridsfromModel finalmodel finalmodel.grids |>bfs exit  }
                
        in
        { movedPaint | move_timer = 0 }
    else if (model.move_timer > stepTime/3 && model.move_timer < (stepTime/3+elapsed))|| (model.move_timer > 2*stepTime/3 && model.move_timer < (2*stepTime/3+elapsed))then
        move model
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
            case player.state of
                Player.Stopped ->
                    valves
                _ ->
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
loadValves : Grids -> List Valve -> Grids
loadValves grids valves =
    List.foldl loadValve grids valves
updateGridsfromModel : Model -> Grids -> Grids
updateGridsfromModel model initialgrids= 
    let
        paints = model.level.paints
        valves = model.level.valves
        stypes =model.level.stypes
        ngrids=List.foldl sendPainttoGrids (loadValves initialgrids valves) paints
    in 
        List.foldl sendStype2Grid ngrids stypes