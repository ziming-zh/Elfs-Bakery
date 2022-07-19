module Model exposing (..)

import Array exposing (Array)
import Color exposing (Color)

import Levels exposing (Level)
import Message exposing (Direction(..), Msg(..), Page(..), Paint, Pos)
import Player
import Random
import Valve exposing (Valve)
import Wall exposing (Wall)
import Valve exposing (VState(..))
import Grid exposing (Grids)
import Grid exposing (initGridsfromLevel,sendPainttoGrids,loadValve,Grid)
import Grid exposing (initGrid)
import Player exposing (Player)
import Task
import Browser.Dom exposing (getViewport)
import Grid exposing (drawWallIndex)
import Grid exposing (sendStype2Grid)
import Message exposing (Paint,Pos,SpecialType(..),Stype,Sstate(..))

type alias Model =
    Mapset
        { win : GaState
        , move_timer : Float
        , levels : List Level
        , guide_levels : List Level
        , level_index : Int
        , level_cleared : List Bool
        , valves_move : Int
        , color_seq : List Color.Color
        , mcolor_seq : List Color.Color
        , history : List (List Paint,List Valve,Player)
        , currentPage : Page
        --, lastMoveDirection : MoveDirection  --merge the direction of the player into Type Player
        --  , stringlevel : StringLevel
        , randomindex : Int
        , windowsize : ( Float, Float )
        }
type GaState
    = Win
    | Lose
    | Playing
    

type alias Flags =
    { levels : Maybe String }


type alias Mapset a =
    { a
        | player : Player
        , wall : Wall
        , valves : List Valve
        , paints : List Paint
        , stypes : List Stype
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

getModel : Int -> Model -> ( Model, Cmd Msg )
getModel k model =
    let
        levels =
            List.drop (k-1) (if model.currentPage == GuidePage then model.guide_levels else model.levels)
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
        (initplayer,stypes) = 
            case List.head levels of
                Just lv-> (lv.player,lv.stypes)
                Nothing -> (Player.init (Pos 0 0) Message.Up,[])
        mapsize = 
            case List.head levels of
                Just lv-> (lv.width,lv.height)
                Nothing -> (0,0)
    in
    ( { player = initplayer
      , wall = wall
      , valves = valves
      , paints = paints
      , grids = initialgrids
      , updatedGrids = initialgrids
      , stypes = stypes
      , dots = []
      , mapSize = mapsize
      , win = Playing
      , levels = model.levels -- important here
      , guide_levels = model.guide_levels
      , move_timer = 0.0
      , level_index = k
      , valves_move = 0
      , history = []
      , currentPage = LevelsPage
      , windowsize = ( 800, 800 )
      , randomindex = 0
      , exit = initGrid exit.x exit.y -- to be imported from the level later
      , color_seq = colorseq
      , mcolor_seq = []
      , level_cleared = model.level_cleared
      }
    , Cmd.batch
        [ --Random.generate RandomLevel (Random.int 0 39),
          Task.perform GetViewport getViewport
        ]
    )

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
        (initplayer,stypes) = 
            case List.head levels of
                Just lv-> (lv.player,lv.stypes)
                Nothing -> (Player.init (Pos 0 0) Message.Up,[])

        mapsize = 
            case List.head levels of
                Just lv-> (lv.width,lv.height)
                Nothing -> (0,0)
    in
    ( { player = initplayer
      , wall = wall
      , valves = valves
      , paints = paints
      , grids = initialgrids
      , stypes= []
      , mapSize = mapsize
      , updatedGrids = initialgrids
      , dots = []
      , win = Playing
      , levels = levels -- important here
      , guide_levels = Levels.initGuide
      , move_timer = 0.0
      , level_index = 0
      , valves_move = 0
      , history = [(paints,valves,initplayer)]
      , currentPage = HomePage
      , windowsize = ( 800, 800 )
      , randomindex = 0
      , exit = initGrid exit.x exit.y -- to be imported from the level later
      , color_seq = colorseq
      , mcolor_seq = []
      , level_cleared = List.repeat 7 False
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
        stypes =model.stypes
        ngrids=List.foldl sendPainttoGrids (loadValves initialgrids valves) paints
    in 
        List.foldl sendStype2Grid ngrids stypes
        -- initialgrids
