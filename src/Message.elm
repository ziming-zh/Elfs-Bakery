module Message exposing (..)

import Browser.Dom exposing (Viewport)
import Levels exposing (EncodeLevel)
type ArrowKey
    = NoKey
    | Space
    | LeftKey
    | RightKey
    | UpKey
    | DownKey
    | F
type alias Block =
    { x : Int
    , y : Int
    }

type MoveDirection
    = Left
    | Right
    | Up
    | Down

type Page
    = GamePage
    | SettingsPage
    | HomePage
    | LevelsPage
    | CakeCollectionPage


type Msg
    = ArrowPressed ArrowKey
    | ArrowReleased ArrowKey
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