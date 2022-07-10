module Wall exposing (Wall, isWall,Wall_col,Wall_row,getWall)

import Array exposing (fromList, get)
import Maybe exposing (withDefault)
import Message exposing (Direction(..), Pos)


type alias Wall_row =
    List (List Bool)


type alias Wall_col =
    List (List Bool)


type alias Wall =
    { col : Wall_col, row : Wall_row }


isWall :Pos -> Direction -> Wall -> Bool
isWall pos dir wall =
    let
        x =
            pos.x

        y =
            pos.y

        wcol =
            wall.col

        wrow =
            wall.row
    in
    case dir of
        Up ->
            getWall wrow x y
        Down ->
            getWall wrow x (y+1)
        Left ->
            getWall wcol x y
        Right -> 
            getWall wcol (x+1) y
        Stop ->
            True
            
getWall: (List (List Bool))-> Int -> Int -> Bool -- a helper function to get wall(x,y)
getWall wall x y =
    get y (fromList wall)
    |> withDefault []
    |> fromList
    |> get x
    |> withDefault True
--confused whether the order of x and y is correct