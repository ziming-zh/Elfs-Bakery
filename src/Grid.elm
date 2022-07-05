module Grid exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Html.Attributes exposing (rows)
import Levels exposing (Level)
import Message exposing (Direction(..), Paint, Pos)
import String exposing (lines)
import Valve exposing (VState(..), Valve)
import Wall exposing (Wall, Wall_col, Wall_row, getWall)


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

isOpen2String: Pos -> Direction -> Grids -> String
isOpen2String pos dir grids =
    let
            
        dirstring =
            case dir of 
                Message.Up ->" Up "
                Message.Down -> " Down "
                Message.Left -> " Left "
                Message.Right -> " Right "
                _ -> " None "
        isopen=getGstate pos grids dir
        isopenstring =
            case isopen of
                Close -> " close "
                FakeClose -> " fake "
                Open -> " Open "
    in
        dirstring++isopenstring
getbugState : Pos -> Grids -> String
getbugState pos grids =
    "\n"++"x: "++String.fromInt pos.x++"y: "++String.fromInt pos.y ++ isOpen2String pos Message.Up grids ++ isOpen2String pos Message.Down grids ++ isOpen2String pos Message.Left grids ++ isOpen2String pos Message.Right grids

getGstate : Pos -> Grids -> Direction -> IsOpen
getGstate pos grids dir =
    let
        x =
            pos.x

        y =
            pos.y

        grid =
            getGrid x y grids

        isopen =
            case grid of
                Nothing ->
                    Close

                Just g ->
                    case dir of
                        Message.Up ->
                            g.gstate.up

                        Message.Down ->
                            g.gstate.down

                        Message.Right ->
                            g.gstate.right

                        Message.Left ->
                            g.gstate.left

                        Message.Stop ->
                            Close
    in
    isopen


getDistance : Pos -> Grids -> Int
getDistance pos grids =
    let
        x =
            pos.x

        y =
            pos.y

        grid =
            getGrid x y grids

        distance =
            case grid of
                Nothing ->
                    -1

                Just g ->
                    Maybe.withDefault -1 g.distance
    in
    distance


type GridType a
    = Paint a
    | Exit
    | Vacant


type alias Grid =
    { pos : Pos
    , gridtype : GridType Paint -- set white default for blockes holes

    -- , color : Color -- set white default for blockes holes
    , gstate : GState
    , distance : Maybe Int
    , renewed : Bool
    }


type alias Grids =
    Array (Array Grid)


initGrid : Int -> Int -> Grid
initGrid y x =
    { pos = { x = x, y = y }, gridtype = Vacant, gstate = { up = Open, down = Open, left = Open, right = Open }, distance = Just 0, renewed = True }


ban : IsOpen -> Direction -> Grid -> Grid
ban isopen dir grid =
    let
        gstate =
            grid.gstate

        newstate =
            case dir of
                Message.Down ->
                    { gstate | down = setGstate gstate.down isopen }

                Message.Up ->
                    { gstate | up = setGstate gstate.up isopen }

                Message.Left ->
                    { gstate | left = setGstate gstate.left isopen  }

                Message.Right ->
                    { gstate | right = setGstate gstate.right isopen }

                _ ->
                    gstate
    in
    { grid | gstate = newstate }

setGstate : IsOpen -> IsOpen -> IsOpen
setGstate isopen change=
    case isopen of
        Close -> isopen
        _ -> change
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


refreshRowGrids : IsOpen -> Int -> Int -> Grids -> Grids
refreshRowGrids isopen x y grids =
    let
        upperblock =
            getGrid (x - 1) y grids

        downblock =
            getGrid x y grids
    in
    case ( upperblock, downblock ) of
        ( Just ub, Just db ) ->
            grids
                |> setGrid (x - 1) y (ban isopen Message.Down ub)
                |> setGrid x y (ban isopen Message.Up db)

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


refreshColumnGrids : IsOpen -> Int -> Int -> Grids -> Grids
refreshColumnGrids isopen y x grids =
    let
        leftblock =
            getGrid x (y - 1) grids

        rightblock =
            getGrid x y grids
    in
    case ( leftblock, rightblock ) of
        ( Just lb, Just rb ) ->
            grids
                |> setGrid x (y - 1) (ban isopen Message.Right lb)
                |> setGrid x y (ban isopen Message.Left rb)
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
        initialgrids =
            Array.fromList (List.map (\x -> Array.fromList (List.map (initGrid x) (List.range 0 (level.width - 1)))) (List.range 0 (level.height - 1)))
                |> loadWall level

        -- |> loadValves level.valves
    in
    initialgrids



-- List.foldl sendPainttoGrids initialgrids paints
-- find out grid
-- List.append
--     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) row))
--     (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (initGrid a)) col))
-- sendPos : Int -> Int -> Bool -> Maybe (Int, Int)
-- sendPos x y iswall =
--     case iswall of
--         True -> Just (x,y)
--         False -> Nothing


zip : List a -> List b -> List ( a, b )
zip u1 u2 =
    List.map2 Tuple.pair u1 u2


drawWallIndex : List Bool -> List Int
drawWallIndex wallline =
    let
        deleteblank =
            \x -> List.filter filtwall x

        filtwall =
            \x ->
                case x of
                    ( False, _ ) ->
                        False
                    _ ->
                        True
    in
    List.indexedMap (\x y -> ( y, x )) wallline
        |> deleteblank
        |> List.filter filtwall
        |> List.unzip
        |> Tuple.second


sendWallLine : ( List Bool, Int ) -> Grids -> Grids
sendWallLine ( liney, x ) grids =
    List.foldl (refreshRowGrids Close x) grids (drawWallIndex liney)


sendWallColumn : ( List Bool, Int ) -> Grids -> Grids
sendWallColumn ( liney, x ) grids =
    List.foldl (refreshColumnGrids Close x) grids (drawWallIndex liney)


loadWall : Level -> Grids -> Grids
loadWall level grids =
    let
        wall =
            level.wall

        indexedrow =
            List.indexedMap (\x y -> ( y, x )) wall.row

        indexedcol =
            List.indexedMap (\x y -> ( y, x )) wall.col

        loadrow =
            List.foldl sendWallLine grids indexedrow

        loadcolumn =
            List.foldl sendWallColumn loadrow indexedcol
    in
    loadcolumn


loadValve : Valve -> Grids -> Grids
loadValve valve grids =
    let
        posx =
            valve.pos.x

        posy =
            valve.pos.y
    in
    -- (x,y,dir)
    case valve.state of
        Valve.Left ->
            resetGrid posx posy grids
            |> refreshRowGrids FakeClose posx (posy - 1)

        Valve.Right ->
            resetGrid posx posy grids
            |> refreshRowGrids FakeClose posx posy 

        Valve.Up ->
            resetGrid posx posy grids
            |> refreshColumnGrids FakeClose posy (posx - 1) 

        Valve.Down ->
            resetGrid posx posy grids
            |> refreshColumnGrids FakeClose posy posx  

resetGrid : Int -> Int -> Grids -> Grids
resetGrid posx posy grids =
    refreshRowGrids Open posx (posy - 1) grids
    |> (refreshRowGrids Open posx posy )
    |> (refreshColumnGrids Open posy (posx - 1)  )
    |> (refreshColumnGrids Open posy posx  )
loadValves : List Valve -> Grids -> Grids
loadValves valves grids =
    List.foldl loadValve grids valves

sendPainttoGrids : Paint -> Grids -> Grids
sendPainttoGrids paint grids =
    let
        posx =
            paint.pos.x

        posy =
            paint.pos.y

        gridline =
            case Array.get posx grids of
                Nothing ->
                    Array.fromList []

                Just gl ->
                    gl
    in
    case Array.get posy gridline of
        Nothing ->
            grids

        Just grid ->
            Array.set posx (Array.set posy { grid | gridtype = Paint paint } gridline) grids


movePaint :  Grids -> Paint ->Paint
movePaint grids paint =
    let
        x =
            paint.pos.x

        y =
            paint.pos.y

        grid =
            getGrid x y grids

        distance =
            getDistance paint.pos grids

        ( dx, dy ) =
            if distance == (getDistance { x = x + 1, y = y } grids + 1) && getGstate paint.pos grids Message.Down == Open then
                ( 1, 0 )

            else if distance == (getDistance { x = x, y = y + 1 } grids + 1) && getGstate paint.pos grids Message.Right == Open then
                ( 0, 1 )

            else if distance == (getDistance { x = x, y = y - 1 } grids + 1)  && getGstate paint.pos grids Message.Left == Open then
                ( 0, -1 )

            else if distance == (getDistance { x = x - 1, y = y } grids + 1)  && getGstate paint.pos grids Message.Up == Open then
                ( -1, 0 )

            else
                ( 0, 0 )
    in
    { paint | pos = { x = x + dx, y = y + dy } }
