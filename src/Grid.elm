module Grid exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Html.Attributes exposing (rows)
import Levels exposing (Level)
import Message exposing (Direction(..), Paint, Pos)
import String exposing (lines)
import Valve exposing (VState(..), Valve)
import Wall exposing (Wall, Wall_col, Wall_row, getWall)
import Html exposing (a)
import Color exposing (..)


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
                    100

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
initGrid x y =
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
        ( Nothing, Just db ) ->
            grids
                |> setGrid x y (ban isopen Message.Up db)
        ( Just ub, Nothing) ->
            grids
                |> setGrid (x - 1) y (ban isopen Message.Down ub)
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
        ( Nothing, Just rb ) ->
            grids
                |> setGrid x y (ban isopen Message.Left rb)
        ( Just lb, Nothing ) ->
            grids
                |> setGrid x (y - 1) (ban isopen Message.Right lb)
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
            refreshRowGrids FakeClose posx (posy - 1) grids

        Valve.Right ->
            refreshRowGrids FakeClose posx posy grids

        Valve.Up ->
            refreshColumnGrids FakeClose posy (posx - 1) grids

        Valve.Down ->
            refreshColumnGrids FakeClose posy posx grids

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

judgeOk : Grids -> Int -> Int -> Bool
judgeOk grids x y =
    case getGrid x y grids of
        Just g -> 
            case g.gridtype of
                Paint _ ->
                    False
                _ -> True
        _ -> False
        
judge1Ok : Grids -> Int -> Int -> Color -> Bool
judge1Ok grids x y color =
    let 
        grid =
            getGrid x y grids
    in
        case grid of
            Just singlegrid ->
                case singlegrid.gridtype of
                    Paint a ->
                        a.color == color
                    _ -> False
            Nothing -> False

getColor : Grids -> Int -> Int -> Color
getColor grids x y =
    let 
        grid =
            getGrid x y grids
    in
        case grid of
            Just singlegrid ->
                case singlegrid.gridtype of
                    Paint a ->
                        a.color 
                    _ -> white
            Nothing -> white
            
mergeColor : Color -> Color -> Color
mergeColor a b =
    if a == b then a
    else if a == white || b == white then white
    else if (a,b) == (lightRed,lightYellow) then orange
    else if (a,b) == (lightYellow,lightRed) then orange
    else if (a,b) == (lightRed,blue) then purple
    else if (a,b) == (blue,lightRed) then purple
    else if (a,b) == (blue,lightYellow) then green
    else if (a,b) == (lightYellow,blue) then green

    else if (a,b) == (blue,purple) then purple
    else if (a,b) == (purple,blue) then purple
    else if (a,b) == (blue,green) then green
    else if (a,b) == (green,blue) then green
    else if (a,b) == (blue,orange) then white
    else if (a,b) == (orange,blue) then white

    else if (a,b) == (lightRed,purple) then purple
    else if (a,b) == (purple,lightRed) then purple
    else if (a,b) == (lightRed,orange) then orange
    else if (a,b) == (orange,lightRed) then orange
    else if (a,b) == (lightRed,green) then white
    else if (a,b) == (green,lightRed) then white

    else if (a,b) == (lightYellow,orange) then orange
    else if (a,b) == (orange,lightYellow) then orange
    else if (a,b) == (lightYellow,green) then green
    else if (a,b) == (green,lightYellow) then green
    else if (a,b) == (lightYellow,purple) then white
    else if (a,b) == (purple,lightYellow) then white

    else white

nodefault : Maybe Grid -> Grid
nodefault grid = 
    case grid of
        Just a -> a
        Nothing -> initGrid 0 0

getPaint : (Int,Int) -> Array Paint -> Paint
getPaint (lx,ly) paints =
    let
        listpaints = Array.toList paints 
        list = List.filter (\xx -> xx.pos == Pos lx ly) listpaints
    in
        case List.head list of 
            Just a -> a
            _ -> {pos = Pos -1 -1,color=Color.lightYellow}

changeSingleColor : (Int,Int) -> Color -> Array Paint -> Array Paint
changeSingleColor (lx,ly) ncolor paints = 
    Array.map 
        (\x ->
            if x.pos /= (Pos lx ly) then  x
            else 
                {pos = x.pos,color = ncolor}
        ) paints

changeColor : Grids -> Color -> Color -> (Int,Int) -> (Int,Int) -> Array Paint -> Array Paint
changeColor grids color lcolor (lx,ly) (dx,dy) paints =
    let
        (nx,ny) = (lx+dx,ly+dy)
        foldFunction = 
            changeColor grids color lcolor (nx,ny)
        npaints = 
            (changeSingleColor (nx,ny) color paints)
        downOk = judge1Ok grids (nx+1) ny lcolor
        leftOk = judge1Ok grids nx (ny-1) lcolor
        rightOk =  judge1Ok grids nx (ny+1) lcolor
        upOk =  judge1Ok grids (nx-1) ny lcolor   
        list = 
            List.concat
            [
            if (dx,dy) /= (-1,0) && getGstate (Pos nx ny) grids Message.Down == Open && downOk then
                [( 1, 0 )]
            else []
            ,
            if (dx,dy) /= (0,-1) && getGstate (Pos nx ny) grids Message.Right == Open && rightOk then
                 [( 0, 1 )]
            else []
            ,
            if (dx,dy) /= (0,1) && getGstate (Pos nx ny) grids Message.Left == Open && leftOk then
                [( 0, -1 )]
            else []
            ,
            if (dx,dy) /= (1,0) && getGstate (Pos nx ny) grids Message.Up == Open && upOk then
               [( -1, 0 )]
            else []
            ]
    in
        List.foldl foldFunction npaints list
movePaint :  Grids -> Int -> Array Paint -> Array Paint
movePaint grids i paints =
    let
    
        defaultPaint= {pos = {x=-1,y=-1},color=Color.lightYellow}
        paint = Maybe.withDefault defaultPaint (Array.get i paints)
        x =
            paint.pos.x

        y =
            paint.pos.y

        grid =
            getGrid x y grids
        downOk = judgeOk grids (x+1) y
        leftOk = judgeOk grids x (y-1)
        rightOk =  judgeOk grids x (y+1)
        upOk =  judgeOk grids (x-1) y 
        distance =
            getDistance paint.pos grids

        ( dx, dy ) =
            if distance == (getDistance { x = x + 1, y = y } grids + 1) && getGstate paint.pos grids Message.Down == Open && downOk then
                ( 1, 0 )

            else if distance == (getDistance { x = x, y = y + 1 } grids + 1) && getGstate paint.pos grids Message.Right == Open && rightOk then
                ( 0, 1 )

            else if distance == (getDistance { x = x, y = y - 1 } grids + 1)  && getGstate paint.pos grids Message.Left == Open && leftOk then
                ( 0, -1 )

            else if distance == (getDistance { x = x - 1, y = y } grids + 1)  && getGstate paint.pos grids Message.Up == Open && upOk then
                ( -1, 0 )

            else
                ( 0, 0 )
    in
    if (dx,dy) /= (0,0) then
        Array.set i { paint | pos = { x = x + dx, y = y + dy } } paints 
    else 
        if distance == (getDistance { x = x + 1, y = y } grids + 1) && getGstate paint.pos grids Message.Down == Open then
            changeColor grids (mergeColor (getColor grids x y) (getColor grids (x+1) y)) (getColor grids (x+1) y) (x,y) (1,0) paints
                |> changeColor grids (mergeColor (getColor grids x y) (getColor grids (x+1) y)) (getColor grids x y) (x+1,y) (-1,0)
        else if distance == (getDistance { x = x, y = y + 1 } grids + 1) && getGstate paint.pos grids Message.Right == Open then
            changeColor grids (mergeColor (getColor grids x y) (getColor grids x (y+1))) (getColor grids x (y+1)) (x,y) (0,1) paints
                |> changeColor grids (mergeColor (getColor grids x y) (getColor grids x (y+1))) (getColor grids x y) (x,y+1) (0,-1)
        else if distance == (getDistance { x = x, y = y - 1 } grids + 1)  && getGstate paint.pos grids Message.Left == Open then
            changeColor grids (mergeColor (getColor grids x y) (getColor grids x (y-1))) (getColor grids x (y-1)) (x,y) (0,-1) paints
                |> changeColor grids (mergeColor (getColor grids x y) (getColor grids x (y-1))) (getColor grids x y) (x,y-1) (0,1)
        else if distance == (getDistance { x = x - 1, y = y } grids + 1)  && getGstate paint.pos grids Message.Up == Open then
            changeColor grids (mergeColor (getColor grids x y) (getColor grids (x-1) y)) (getColor grids (x-1) y) (x,y) (-1,0) paints   
                |> changeColor grids (mergeColor (getColor grids x y) (getColor grids (x-1) y)) (getColor grids x y) (x-1,y) (1,0)
        else paints