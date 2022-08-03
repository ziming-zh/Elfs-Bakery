module Wall exposing (Wall)

{-| This library defines the Wall type.

# Data Type
@docs Wall

-}
import Message exposing (Direction(..))


type alias Wall_row =
    List (List Bool)


type alias Wall_col =
    List (List Bool)

{-| Wall datatype contains rows and columns, and both the row and column rows are 2D bools.
-}
type alias Wall =
    { col : Wall_col, row : Wall_row }

