module Message exposing (..)

import Browser.Dom exposing (Viewport)
import Color exposing (Color)
import Levels exposing (EncodeLevel)


type alias Pos =
    { x : Int
    , y : Int
    }


stepTime : Int
stepTime =
    1000
type alias Paint =
    { pos : Pos, dir : MoveDirection, color : Color }


type alias Paints =
    List Paint


grid_size : Int
grid_size =
    10


type MoveDirection
    = Left
    | Right
    | Up
    | Down
    | Stop


type Page
    = GamePage
    | SettingsPage
    | HomePage
    | LevelsPage
    | CakeCollectionPage


type Msg
    = ArrowPressed MoveDirection
    | Tick Float
    | GetViewport Viewport
    | Resize Int Int
    | Pause
    | Resume
    | Move MoveDirection
    | Undo
    | ShowPage Page
    | RestartLevel
    | LoadNextLevel
    | LoadLevel EncodeLevel
    | AddLevel EncodeLevel
    | RemoveLevel EncodeLevel
      -- | ChangeLevelFromUserInput String
      -- | AddLevelFromUserInput
    | RandomLevel Int
    | None


key : Int -> Msg
key keycode =
    case keycode of
        37 ->
            ArrowPressed Left

        39 ->
            ArrowPressed Right

        38 ->
            ArrowPressed Up

        40 ->
            ArrowPressed Down

        _ ->
            ArrowPressed Stop
