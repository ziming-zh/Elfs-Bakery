module Model exposing (..)

import Array exposing (Array)
import Color exposing (Color)

import Levels exposing (Level)
import Message exposing (Direction(..), Msg(..), Page(..), Paint, Pos)
import Player
import Random
import Valve exposing (Valve)
import Wall exposing (Wall)
import Valve exposing (initValve,VState(..))
import Grid exposing (Grids)
import Grid exposing (initGridsfromLevel,sendPainttoGrids,loadValve,Grid)
import Grid exposing (initGrid)
import Player exposing (Player)
import Levels exposing (initLevel1) 
import Task
import Browser.Dom exposing (getViewport)

type alias Model =
    Mapset
        { win : Bool
        , move_timer : Float
        , levels : List Level
        , level_index : Int
        , valves_move : Int
        , color_seq : List Color.Color
        , mcolor_seq : List Color.Color
        , history : List GameState
        , currentPage : Page
        --, lastMoveDirection : MoveDirection  --merge the direction of the player into Type Player
        --  , stringlevel : StringLevel
        , randomindex : Int
        , windowsize : ( Float, Float )
        }


type alias Flags =
    { levels : Maybe String }


type alias Mapset a =
    { a
        | player : Player
        , wall : Wall
        , valves : List Valve
        , paints : List Paint
        , updatedGrids : Grids
        , grids : Grids
        , dots : List Pos --what is dots
        , mapSize : ( Int, Int )
        , exit :Grid
    }


type alias GameState =
    { gamestate : CurState
    , laststate : CurState

    {- undo to the previous scenario -}
    }


type alias CurState =
    { player : Player
    , valves : List Valve
    , paints : List Paint
    }


{-| load level, new or existing
set level\_index in each case
-}

{-
checkAndLoadGameWithLevel : EncodeLevel -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkAndLoadGameWithLevel encodedLevel ( model, cmd ) =
    let
        isExistingLevel =
            LevelSeq.isDuplicate encodedLevel model.levels
    in
    if isExistingLevel then
        loadGameWithExistingLevel encodedLevel ( model, cmd )

    else
        loadGameWithNewLevel encodedLevel ( model, cmd )


loadGameWithExistingLevel : EncodeLevel -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadGameWithExistingLevel level ( model, cmd ) =
    let
        modelWithGame =
            loadGameWithLevel level model

        levelIndex =
            LevelSeq.getIndexOf level model.levels
    in
    ( { modelWithGame
        | level_index = levelIndex
      }
    , cmd
    )


loadGameWithNewLevel : EncodeLevel -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadGameWithNewLevel level ( model, cmd ) =
    let
        modelWithGame =
            loadGameWithLevel level model

        newLevels =
            LevelSeq.prependLevel level model.levels
    in
    ( { modelWithGame
        | levels = newLevels
        , level_index = 0
      }
    , Cmd.batch
        [ cmd

        -- , Storage.storeLevels newLevels
        ]
    )



{- Loading Levels to activate the game

   loadGameWithLevel : Level -> Model -> Model
   loadGameWithLevel encodedLevel model =
       let
           game =
               decodeLevel encodedLevel
       in
           { player = game.player
           , wall = game.walls
           , paints = game.paints
           , valves = game.valves
           , dots = game.dots
           , mapSize = game.gameSize
           , win = False
           , lastMoveDirection = Up
           , elapse = 0.0
           , levels = model.levels
           , currentLevel = encodedLevel
           , level_index = model.level_index
           , valves_move = 0
           , history = []
           , currentPage = GamePage
           , windowsize = model.windowsize
           , randomindex = model.randomindex
           }

-}


loadGameWithLevel : EncodeLevel -> Model -> Model
loadGameWithLevel encodedLevel model =
    model
-}

initModel : ( Model, Cmd Msg )
initModel =
    let
        levels =
            Levels.getInitialLevels
        initialgrids = 
            case List.head levels of
                Just lv-> initGridsfromLevel lv
                Nothing -> Array.fromList []
        (wall,valves,paints) = 
            case List.head levels of
                Just lv-> (lv.wall,lv.valves,lv.paints)
                Nothing ->({col=[],row=[]},[], [])
        (exit, colorseq) = 
            case List.head levels of
                Just lv-> (lv.exit,lv.colorseq)
                Nothing -> (Pos -1 -1,[])       
    in
    ( { player = Player.init
      , wall = wall
      , valves = valves
      , paints = paints
      , grids = initialgrids
      , updatedGrids = initialgrids
      , dots = []
      , mapSize = (0,0)
      , win = False
      , levels = levels -- important here
      , move_timer = 0.0
      , level_index = 0
      , valves_move = 0
      , history = []
      , currentPage = HomePage
      , windowsize = ( 800, 800 )
      , randomindex = 0
      , exit = initGrid exit.x exit.y -- to be imported from the level later
      , color_seq = colorseq
      , mcolor_seq = []
      }
    , Cmd.batch
        [ Random.generate RandomLevel (Random.int 0 39)
        , Task.perform GetViewport getViewport
        ]
    )
loadValves : Grids -> List Valve -> Grids
loadValves grids valves =
    List.foldl loadValve grids valves
updateGridsfromModel : Model -> Grids -> Grids
updateGridsfromModel model initialgrids= 
    let
        paints = model.paints
        valves = model.valves
    in
        List.foldl sendPainttoGrids (loadValves initialgrids valves) paints
        -- initialgrids