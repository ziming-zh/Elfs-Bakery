module Grid exposing (..)
import Message exposing (Pos)
import Color exposing (Color)
import Levels exposing (Level)
import Array exposing (Array)

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
type alias Grids = Array (Array Grid)

initGrid : Int -> Int  -> Grid
initGrid y x = 
    { pos = {x=x,y=y}, color = Color.white , gstate = {up=Open, down=Open, left=Open,right= Open} }

banbottom : Grid -> Grid
banbottom grid = 
    let 
        gstate = grid.gstate
        newstate = {gstate | down = Close}

    in
    {grid | gstate = newstate}
banTop : Grid -> Grid
banTop grid = 
    let 
        gstate = grid.gstate
        newstate = {gstate | up = Close}

    in
    {grid | gstate = newstate}
banLeft : Grid -> Grid
banLeft grid = 
    let 
        gstate = grid.gstate
        newstate = {gstate | left = Close}

    in
    {grid | gstate = newstate}
banRight : Grid -> Grid
banRight grid = 
    let 
        gstate = grid.gstate
        newstate = {gstate | right = Close}

    in
    {grid | gstate = newstate}



getGrid : Int -> Int -> Grids -> Maybe Grid
getGrid x y grids = 
    let
        gridline = (Array.get x grids)
    in
        case gridline of 
            Nothing -> Nothing
            Just gl -> Array.get y gl

setGrid : Int -> Int -> Grid -> Grids -> Grids
setGrid x y newgrid grids = 
    let
        gridline = (Array.get x grids)
    in
        case gridline of 
            Nothing -> grids
            Just gl -> 
                Array.set x (Array.set y newgrid gl) grids

refreshRowGrids : Int -> Int -> Grids -> Grids
refreshRowGrids x y grids = 
    let
        upperblock = getGrid (x-1) y grids
        downblock = getGrid x y grids
    in
        case (upperblock,downblock) of
            (Just ub,Just db) ->
                grids 
                |> setGrid (x-1) y (banbottom ub)
                |> setGrid x y (banTop ub) 
            _ -> grids
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
        leftblock = getGrid x (y-1) grids
        rightblock = getGrid x y grids
    in
        case (leftblock,rightblock) of
            (Just lb,Just rb) ->
                grids 
                |> setGrid x (y-1) (banRight lb)
                |> setGrid x y (banLeft rb)
            _ -> grids
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
initGridsfromLevel level= 
    let
        row = level.wall.row
        col = level.wall.col
        initialgrids = Array.fromList (List.map (\x -> Array.fromList (List.map (initGrid x) (List.range 1 level.width))) (List.range 1 level.height))
        
        
    in
        initialgrids
        
        -- find out grid
        
        -- List.append
        --     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) row))
        --     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) col))
        