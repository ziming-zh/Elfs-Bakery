module Grid exposing (..)
import Message exposing (Pos)
import Color exposing (Color)
import Levels exposing (Level)
import Array exposing (Array)
import Message exposing (Paint)
import Wall exposing (Wall)

type IsOpen 
    = Open
    | FakeClose
    | Close

    

type alias GState =
    { up : IsOpen
    , down : IsOpen
    , left : IsOpen
    , right : IsOpen
    }

type GridType a = 
    Paint a
    | Exit
    | Vacant

type alias Grid = 
    { pos : Pos
    , gridtype : GridType Paint -- set white default for blockes holes
    , gstate : GState
    , distance : Maybe Int
    , renewed : Bool
    }

initGrid : Int -> Int  -> Grid
initGrid y x = 
    { pos = {x=x,y=y}, gridtype = Vacant , gstate = {up=Open, down=Open, left=Open,right= Open} , distance = Just 1, renewed = True}

sendPainttoGrids :  Paint -> Grids -> Grids
sendPainttoGrids  paint grids = 
    let 
        posx = paint.pos.x
        posy = paint.pos.y
        gridline = case (Array.get posx grids) of
            Nothing -> Array.fromList []
            Just gl ->gl
    in
        case (Array.get posy gridline) of
            Nothing -> grids
            Just grid -> Array.set posx (Array.set posy {grid|gridtype=Paint paint} gridline) grids



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

type alias Grids = Array (Array Grid)
refreshRowGrids : Int -> Int -> Grids -> Grids
refreshRowGrids x y grids = 
    let 
        uppergridline = (Array.get (x-1) grids)
        downgridline = (Array.get x grids)
        upperblock = case (uppergridline,downgridline) of
            (Just a,_) -> Array.get y a
            _ -> Nothing
        downblock = case (uppergridline,downgridline) of
            (_, Just b) -> Array.get y b
            _ -> Nothing

        newgrid = 
            case (uppergridline, downgridline, downblock) of
                
                (Just ugl, Just dgl, Just db) -> 
                    case upperblock of 
                        Nothing -> 
                            Array.set x ( Array.set y (banTop db) dgl)grids
                        Just bb ->
                            Array.set x ( Array.set y (banTop db) dgl) grids
                            |>  Array.set (x-1) ( Array.set y (banbottom bb) ugl)

                _ -> grids
    in
        newgrid

refreshColumnGrids : Int -> Int -> Grids -> Grids
refreshColumnGrids x y grids = 
    let 
        gridline = case (Array.get x grids) of
            Nothing -> (Array.fromList [])
            Just gl -> gl
        
        leftblock = Array.get (y-1) gridline
        rightblock = Array.get (y) gridline

        newgrid = 
            case (leftblock,rightblock) of
                
                (Just a, Just b) ->
                    Array.set x ( Array.set (y-1) (banRight a) gridline) grids
                    |>  Array.set x ( Array.set y (banLeft b) gridline)
                (Just a, Nothing) ->
                    Array.set x ( Array.set (y-1) (banRight a) gridline) grids
                (Nothing, Just b) ->
                    Array.set x ( Array.set y (banLeft b) gridline) grids
                _ -> grids
    in
        newgrid

unrenewGrids : Grids -> Grids
unrenewGrids grids = 
    Array.map (Array.map (\x -> {x| renewed = False})) grids

initGridsfromLevel : Level -> Grids
initGridsfromLevel level= 
    let
        row = level.wall.row
        col = level.wall.col
        paints = level.paints
        initialgrids = Array.fromList (List.map (\x -> Array.fromList (List.map (initGrid x) (List.range 1 level.width))) (List.range 1 level.height))   
        gridswithPaints = List.foldl sendPainttoGrids initialgrids paints
    in
        gridswithPaints
        -- find out grid
        
        -- List.append
        --     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) row))
        --     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) col))

loadWall : Grids -> Wall -> Grids
loadWall grids wall = grids

    
