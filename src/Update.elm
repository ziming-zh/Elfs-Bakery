module Update exposing (update)
import Message exposing (Msg(..),ArrowKey(..))

import Color exposing (Color)

import Browser.Dom exposing (getViewport)

import Task
import Levels exposing (Level)
import LevelSeq exposing (LevelSeq)

import Model exposing (Model)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame { model | elapse = elapsed }
        Resize width height ->
            ( { model | windowsize = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        GetViewport { viewport } ->
            ( { model
                | windowsize =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none ) 

        Move direction ->
            let
                newModel =
                    model
                        |> addToHistory
                        |> movePlayer direction
                        |> moveBoxes direction
                        |> checkCollisions
                        |> checkIfWin
                        |> storeLevelData
                        |> Maybe.withDefault model
            in
                if newModel.isWin then
                    ( newModel
                    , Storage.storeLevelsData newModel.levelsData
                    )
                else
                    ( newModel, Cmd.none )

        Undo ->
            ( model
                |> undoLastMove
            , Cmd.none
            )

        ShowPage page ->
            Router.go page model

        RestartLevel ->
            loadLevelById model.currentEncodedLevel model

        LoadNextLevel ->
            loadLevelByIndex (model.currentLevelIndex + 1) model

        LoadLevel levelId ->
            loadLevelById levelId model

        AddLevel levelId ->
            let
                newModel =
                    { model | levels = LevelSeq.appendLevel levelId model.levels }
            in
                ( newModel
                , Storage.storeLevels newModel.levels
                )

        RemoveLevel levelId ->
            let
                newModel =
                    { model | levels = LevelSeq.removeLevel levelId model.levels }
            in
                ( newModel
                , Storage.storeLevels newModel.levels
                )
        

        ArrowPressed Space ->   
            if model.state == Begining || model.state == Ending then 
                ( model , Cmd.none )
            else
            if model.state /= Changing then
                ( { model | state = 
                    if model.state == Playing then Paused
                    else if model.state == Paused then Playing 
                    else GG }, Cmd.none )
            else ( model , Cmd.none )

        ArrowPressed F ->
            ( model , Cmd.none ) |> updateState True

        ArrowPressed arrow ->
            ( updatePlate1 arrow model , Cmd.none )

        ArrowReleased arrow ->
            ( updatePlate2 arrow model , Cmd.none )

        Start ->
            ( model_init 2 , Task.perform GetViewport getViewport )

        Pause ->
            if model.state /= Changing && model.state /= Ending && model.state /= Begining then
                ( { model | state = Paused } , Cmd.none )
            else ( model , Cmd.none )
        
        Resume ->
            ( { model | state = Playing } , Cmd.none )

        RandomLevel number ->
            ( { model | randomLevelIndex = number }
            , Cmd.none
            )
        None -> 
            ( model, Cmd.none)

