module Message exposing (..)

import Browser.Dom exposing (Viewport)
import Color exposing (Color)



type alias Pos =
    { x : Int
    , y : Int
    }


stepTime : Float
stepTime =
    1000
type alias Paint =
    { pos : Pos, color : Color }



type Direction
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
    | ChoicePage
    | GuidePage
    | CollectionPage


type Msg
    = ArrowPressed Direction
    | Tick Float
    | GetViewport Viewport
    | Resize Int Int
    | Pause
    | Resume
    | Move Direction
    | Undo
    | ShowPage Page
    | RestartLevel
    | LoadNextLevel
    | LoadLevel Int
    | Load Page
    | RandomLevel Int
    | Retry
    | None
    | ChoiceInfo Int


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


type SpecialType
    = Chocolate
    | Vanilla
type alias Stype =
    { content : SpecialType, state : Sstate, pos : Pos,target: Int }
type Sstate
    = Moving
    | Still Int
    | SExit Int