module Wall exposing (Wall)
{-|This library defines the Wall type.
-}
import Message exposing (Direction(..))


type alias Wall_row =
    List (List Bool)


type alias Wall_col =
    List (List Bool)


type alias Wall =
    { col : Wall_col, row : Wall_row }

