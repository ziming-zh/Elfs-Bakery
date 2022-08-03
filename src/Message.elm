module Message exposing (Pos, stepTime, Paint, Direction(..), Page(..), Msg(..), key, SpecialType(..), Stype, Sstate(..))
{-| This library defines all the basic parameters and basic datatypes, so everything has to be exposed here.

# Function 
@docs key

# Data Type
@docs Pos, Paint, Direction, Page, Msg, SpecialType, Stype, Sstate

# Value
@docs stepTime

-}
import Browser.Dom exposing (Viewport)
import Color exposing (Color)


{-| (x,y) means row x and column y.
-}
type alias Pos =
    { x : Int
    , y : Int
    }

{-| the time for each step. This makes the game runs at the same rate on different computers
-}
stepTime : Float
stepTime =
    1000

{-| The cream on the map. pos is its position in the map, color means is its color.
-}
type alias Paint =
    { pos : Pos, color : Color }


{-| Directions
-}
type Direction
    = Left
    | Right
    | Up
    | Down
    | Stop

{-| Different Pages. HomePage: the beginning of the game, can select guide or level options.
LevelsPage: Play page. Choice Page: choose levels and display level infos. GuidePage: guidance that help the player get familiar
with the operations. CollectionPage: exhibit the rewards and the progress of the game.
-}
type Page
    = HomePage
    | LevelsPage
    | ChoicePage
    | GuidePage
    | CollectionPage

{-| Different messages. ArrowPressed gives the direction. Tick tells the time. GetViewport gets the size of the website.
Resize resized the website. Undo undoes the last operation on the valves. LoadNextLevel loads the next level of the game.
LoadLevel loads the corresponding level. Load Page loads the corresponding page. RandomLevel generates a random level. Choice
Info gives the information about the level in the choice page.
-}
type Msg
    = ArrowPressed Direction
    | Tick Float
    | GetViewport Viewport
    | Resize Int Int
    | Undo
    | LoadNextLevel
    | LoadLevel Int
    | Load Page
    | RandomLevel Int
    | ChoiceInfo Int

{-| convert the keycode subscribed to a message
-}
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

{-| There are two types of toppings: Chocolate and Vanilla
-}
type SpecialType
    = Chocolate
    | Vanilla

{-| Toppings on the cake. content is its type, state tells its movement, 
pos is the position and target is which layer it is supposed to be added on.
-}
type alias Stype =
    { content : SpecialType, state : Sstate, pos : Pos,target: Int }
{-| state of the toppings. Moving: flowing with the cream; Still: not activated; SExit: reach the exit
-}
type Sstate
    = Moving
    | Still Int
    | SExit Int