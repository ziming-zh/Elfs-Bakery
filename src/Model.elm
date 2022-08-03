module Model exposing (Model,getModel,initModel,GaState(..))
{-| This library defines the Model
-}
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
import Grid exposing (initGridsfromLevel,Grid)
import Player exposing (Player)
import Task
import Browser.Dom exposing (getViewport)
import Message exposing (Paint,Pos,SpecialType(..),Stype,Sstate(..))
{-| Model data type includes win: the state of the game, move_timer: a moving timer, 
levels: store all the levels in the list, guide_levels: store all the guide levels inside the list,
level_index: the index of the level, level_cleared: record the progress of the game,
mcolor_seq: current progress of the cake, history: the last game state before the valve is operated on,
updatedGrids: always updating, grids: the initial grids that only has predefined color, walls and valves,
level: the current level.
-}
type alias Model =
        { win : GaState
        , move_timer : Float
        , levels : List Level
        , guide_levels : List Level
        , level_index : Int
        , level_cleared : List Bool
        , mcolor_seq : List Color.Color
        , history : List History
        , currentPage : Page
        , windowsize : ( Float, Float )
        , updatedGrids : Grids
        , grids : Grids
        , mapSize : ( Int, Int )
        , level : Level
        }
{-| GaState is the state of the game.
-}
type GaState
    = Win
    | Lose
    | Playing
type alias History =
    {
        paints: List Paint,
        valves: List Valve,
        player: Player,
        stypes: List Stype
    }





{-| return the corresponding guide/game level.
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
        (wall,valves,paints) = ({col=[],row=[]},[], [])
        (exit, colorseq) = (Pos -1 -1,[])       
        (initplayer,stypes) = (Player.init (Pos 0 0) Message.Up,[])
        initlevel: Level
        initlevel = 
            { player = initplayer
            , wall = wall
            , valves = valves
            , paints = paints
            , exit = Pos -1 -1
            , colorseq = colorseq
            , stypes = stypes
            , width = 0
            , height = 0
            , id = 0
                    }
        level=
            case List.head levels of
                Just lv-> lv
                Nothing -> 
                    initlevel
                    
        mapsize = 
            case List.head levels of
                Just lv-> (lv.width,lv.height)
                Nothing -> (0,0)
    in
    ( { level=level
      , updatedGrids = initialgrids
      , mapSize = mapsize
      , win = Playing
      , grids = initialgrids
      , levels = model.levels -- important here
      , guide_levels = model.guide_levels
      , move_timer = 0.0
      , level_index = k
      , history = []
      , currentPage = LevelsPage
      , windowsize = ( 800, 800 )
      , mcolor_seq = []
      , level_cleared = model.level_cleared
      }
    , Cmd.batch
        [ --Random.generate RandomLevel (Random.int 0 39),
          Task.perform GetViewport getViewport
        ]
    )
{-| initializew the model
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
        (wall,valves,paints) = ({col=[],row=[]},[], [])
        (exit, colorseq) = (Pos -1 -1,[])     
        (initplayer,stypes) =  (Player.init (Pos 0 0) Message.Up,[])
        initlevel: Level
        initlevel = 
            { player = initplayer
            , wall = wall
            , valves = valves
            , paints = paints
            , exit = Pos -1 -1
            , colorseq = colorseq
            , stypes = stypes
            , width = 0
            , height = 0
            , id = 0
                    }
        level=
            case List.head levels of
                Just lv-> lv
                Nothing -> 
                    initlevel
        mapsize = 
            case List.head levels of
                Just lv-> (lv.width,lv.height)
                Nothing -> (0,0)
    in
    ( { level=level
      , grids = initialgrids
      , mapSize = mapsize
      , updatedGrids = initialgrids
      , win = Playing
      , levels = levels -- important here
      , guide_levels = Levels.initGuide
      , move_timer = 0.0
      , level_index = 0
      , history = [{paints=paints,valves=valves,player=initplayer,stypes=stypes}]
      , currentPage = HomePage
      , windowsize = ( 800, 800 )
      , mcolor_seq = []
      , level_cleared = List.repeat 7 False
      }
    , Cmd.batch
        [ Random.generate RandomLevel (Random.int 0 39)
        , Task.perform GetViewport getViewport
        ]
    )

