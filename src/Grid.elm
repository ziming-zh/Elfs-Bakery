module Grid exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Html.Attributes exposing (rows)
import Levels exposing (Level)
import Message exposing (Direction(..), Pos)
import Valve exposing (VState(..))
import Wall exposing (Wall, Wall_col, Wall_row, getWall)
import String exposing (lines)


type IsOpen
    = Open
    | Close


type alias GState =
    { up : IsOpen
    , down : IsOpen
    , left : IsOpen
    , right : IsOpen
    }


type alias Grid =
    { pos : Pos
    , color : Color -- set white default for blockes holes
    , gstate : GState
    }


type alias Grids =
    Array (Array Grid)


initGrid : Int -> Int -> Grid
initGrid y x =
    { pos = { x = x, y = y }, color = Color.white, gstate = { up = Open, down = Open, left = Open, right = Open } }


ban : Direction -> Grid -> Grid
ban dir grid =
    let
        gstate =
            grid.gstate

        newstate =
            case dir of
                Message.Down ->
                    { gstate | down = Close }

                Message.Up ->
                    { gstate | up = Close }

                Message.Left ->
                    { gstate | left = Close }

                Message.Right ->
                    { gstate | right = Close }

                _ ->
                    gstate
    in
    { grid | gstate = newstate }


getGrid : Int -> Int -> Grids -> Maybe Grid
getGrid x y grids =
    let
        gridline =
            Array.get x grids
    in
    case gridline of
        Nothing ->
            Nothing

        Just gl ->
            Array.get y gl


setGrid : Int -> Int -> Grid -> Grids -> Grids
setGrid x y newgrid grids =
    let
        gridline =
            Array.get x grids
    in
    case gridline of
        Nothing ->
            grids

        Just gl ->
            Array.set x (Array.set y newgrid gl) grids


refreshRowGrids : Int -> Int -> Grids -> Grids
refreshRowGrids x y grids =
    let
        upperblock =
            getGrid (x - 1) y grids

        downblock =
            getGrid x y grids
    in
    case ( upperblock, downblock ) of
        ( Just ub, Just db ) ->
            grids
                |> setGrid (x - 1) y (ban Message.Down ub)
                |> setGrid x y (ban Message.Up ub)

        _ ->
            grids



-- let
--     uppergridline = (Array.get (x-1) grids)
--     downgridline = (Array.get x grids)
--     upperblock = case (uppergridline,downgridline) of
--         (Just a,_) -> Array.get y a
--         _ -> Nothing
--     downblock = case (uppergridline,downgridline) of
--         (_, Just b) -> Array.get y b
--         _ -> Nothing
--     newgrid =
--         case (uppergridline, downgridline, downblock) of
--             (Just ugl, Just dgl, Just db) ->
--                 case upperblock of
--                     Nothing ->
--                         Array.set x ( Array.set y (banbottom db) dgl)grids
--                     Just bb ->
--                         Array.set x ( Array.set y (banbottom db) dgl) grids
--                         |>  Array.set (x-1) ( Array.set y (banTop bb) ugl)
--             _ -> grids
-- in
--     newgrid


refreshColumnGrids : Int -> Int -> Grids -> Grids
refreshColumnGrids x y grids =
    let
        leftblock =
            getGrid x (y - 1) grids

        rightblock =
            getGrid x y grids
    in
    case ( leftblock, rightblock ) of
        ( Just lb, Just rb ) ->
            grids
                |> setGrid x (y - 1) (ban Message.Right lb)
                |> setGrid x y (ban Message.Left rb)

        _ ->
            grids



-- let
--     gridline = case (Array.get x grids) of
--         Nothing -> (Array.fromList [])
--         Just gl -> gl
--     leftblock = Array.get (y-1) gridline
--     rightblock = Array.get (y) gridline
--     newgrid =
--         case (leftblock,rightblock) of
--             (Just a, Just b) ->
--                 Array.set x ( Array.set (y-1) (banRight a) gridline) grids
--                 |>  Array.set x ( Array.set y (banLeft b) gridline)
--             (Just a, Nothing) ->
--                 Array.set x ( Array.set (y-1) (banRight a) gridline) grids
--             (Nothing, Just b) ->
--                 Array.set x ( Array.set y (banLeft b) gridline) grids
--             _ -> grids
-- in
--     newgrid


initGridsfromLevel : Level -> Grids
initGridsfromLevel level =
    let
        row =
            level.wall.row

        col =
            level.wall.col

        initialgrids =
            Array.fromList (List.map (\x -> Array.fromList (List.map (initGrid x) (List.range 0 (level.width-1)))) (List.range 0 (level.height-1)))
            |> loadWall level
    in
    initialgrids



-- find out grid
-- List.append
--     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) row))
--     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) col))
-- sendPos : Int -> Int -> Bool -> Maybe (Int, Int)
-- sendPos x y iswall = 
--     case iswall of
--         True -> Just (x,y)
--         False -> Nothing

zip : List a -> List b -> List (a,b)
zip u1 u2 = List.map2 Tuple.pair u1 u2

drawWallIndex : List Bool -> List Int
drawWallIndex wallline = 
    let
        deleteblank = (\ x-> List.filter filtwall x)
        
        filtwall = (\x -> case x of
            (False,_) -> False 
            _ -> True)
    in
        List.indexedMap (\x y -> (y, x)) wallline
        |> deleteblank
        |> List.filter filtwall
        |> List.unzip
        |> Tuple.second

sendWallLine : (List Bool, Int) -> Grids -> Grids
sendWallLine (liney,x) grids = 
        List.foldl (refreshRowGrids x) grids (drawWallIndex liney)

sendWallColumn : (List Bool, Int) -> Grids -> Grids
sendWallColumn (liney,x) grids = 
        List.foldl (refreshRowGrids x) grids (drawWallIndex liney)

loadWall : Level -> Grids -> Grids
loadWall level grids =
    let
        wall = level.wall
        row = wall.row
        col =wall.col
        indexedrow = List.indexedMap (\x y -> (y, x)) row
        indexedcol = List.indexedMap (\x y -> (y, x)) col
        -- List.foldl (List.foldl refreshColumnGrids) grids (List.indexedMap (List.indexedMap sendPos) row)
        -- (List.indexedMap (\a -> List.indexedMap (initGrid a)) row)

        loadrow = List.foldl (sendWallLine) grids indexedrow
        loadcolumn = List.foldl (sendWallColumn) loadrow indexedcol
    in
        loadcolumn

-- loadWall : Level -> Grids -> Grids
-- loadWall level grids =
--     let
--         wall=level.wall
--         row =
--             wall.row

--         col =
--             wall.col

--         w =
--             level.width

--         h =
--             level.height

--         newGrids =
--             close (Row row) 0 0 h w grids

--         newnewGrids =
--             close (Col col) 0 0 h w newGrids
--     in
--         newnewGrids


-- close : WallType -> Int -> Int -> Int -> Int -> Grids -> Grids
-- close wall x y xlim ylim grids =
--     if y < ylim then
--         isWall grids wall x y
--             |> close wall x (y + 1) xlim ylim

--     else if x < (xlim - 1) then
--         isWall grids wall x 0
--             |> close wall (x + 1) 0 xlim ylim

--     else
--         grids


-- isWall : Grids -> WallType -> Int -> Int -> Grids
-- isWall grids wall x y =
--     case wall of
--         Row row ->
--             if getWall row x y then
--                 refreshRowGrids x y grids

--             else
--                 grids

--         Col col ->
--             if getWall col x y then
--                 refreshColumnGrids x y grids

--             else
--                 grids


type WallType
    = Row Wall_row
    | Col Wall_col
