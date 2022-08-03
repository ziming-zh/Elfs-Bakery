module DBFS exposing (bfs,get)

{-|This library use bfs algorithm to calclulate the distance 
between each grid and the exit grid.

# Function
@docs bfs, get

-}

import Array exposing (Array)
import Grid exposing (Grid, Grids, IsOpen(..), initGrid)
import Html exposing (a)
import Svg.Attributes exposing (y)




{-| get xth grid in an array of grid.
    
    gett 2 (fromList [grid1,grid2]) = grid2 
-}

gett : Int -> Array Grid -> Grid
gett x grids =
    case Array.get x grids of
        Just a ->
            a

        Nothing ->
            initGrid -1 -1

{-| get the (x,y)th grid of Grids and return the grid's
renewed. (Check whether (x,y) has been visited or not)
-}

getBool : (Int,Int) -> Grids -> Bool
getBool (x,y) grids = 
    let
        grid = get grids x y
    in
        grid.renewed

{-| get the (x,y)th grid of Grids and set the renewed of
the grid to be true. (mark the grid as visited)
-}        

setBool : (Int,Int) -> Grids -> Grids
setBool (x,y) grids = 
    let
        grid = get grids x y
        ngrid = Grid grid.pos grid.gridtype grid.gstate grid.distance True grid.stype
    in
        Array.set x ( Array.set y ngrid (getrow x grids) ) grids

{-| get xth row grid as an array of grid
    
    getrow 2 [[grid1,grid2,grid3],[grid4,grid5,grid6]]
        = [grid4,grid5,grid5]
-}

getrow : Int -> Grids -> Array Grid
getrow x grids =
    case Array.get x grids of
        Just a ->
            a

        Nothing ->
            Array.fromList [ initGrid -1 -1 ]

{-| get the grid at (x,y) of the grids

    get [[grid1,grid2,grid3],[grid4,grid5,grid6]] 1 2 
        = grid2
-}
get : Grids -> Int -> Int -> Grid
get grids x y =
    getrow x grids
        |> gett y


{-| Input a grid's position, check whether we can walk into adjacent
grids and return the grids that can be walked directly from the gird.
-}
checkSurround : Grids -> Int -> Int -> Bool -> Grid -> List Grid
checkSurround grids n m tp grid =
    let
        x =
            grid.pos.x

        y =
            grid.pos.y

        distance =
            case grid.distance of
                Just a ->
                    a + 1

                Nothing ->
                    0

        check state = 
            if tp then
                state == Open 
            else
                state /= Close 

        nqueue =
            List.concat
                [ if x > 0 && check grid.gstate.up && not (getBool (x-1,y) grids) then
                    [ get grids (x - 1) y ]

                  else
                    []
                , if x < (n - 1) && check grid.gstate.down && not (getBool (x+1,y) grids) then
                    [ get grids (x + 1) y ]

                  else
                    []
                , if y > 0 && check grid.gstate.left && not (getBool (x,y-1) grids) then
                    [ get grids x (y - 1) ]

                  else
                    []
                , if y < (m - 1) && check grid.gstate.right && not (getBool (x,y+1) grids) then
                    [ get grids x (y + 1) ]

                  else
                    []
                ]

      --  nnqueue = List.filter (\xx -> xx.distance == Nothing) nqueue

        nnnqueue =
            List.map (
                \xx -> Grid xx.pos xx.gridtype xx.gstate (Just distance) True xx.stype
             ) nqueue
    in
    nnnqueue

{-| update the distances of the grids in this layer of bfs algorithm.
-}
update :Grids -> Array Grid -> (Grids,Array Grid)
update grids queue =
    if Array.length queue > 0 then
        let
            grid =
                gett 0 queue
            x =
                grid.pos.x

            y =
                grid.pos.y

            lgrid = get grids x y

            (ngrids,elem) =
                if lgrid.renewed || ( lgrid.distance /= Nothing && lgrid.distance /= grid.distance ) then 
                    (grids,[])
                else
                    (Array.set x (Array.set y grid (getrow x grids)) grids,[grid])
            (agrids,aqueue) = 
                update ngrids (Array.slice 1 (Array.length queue) queue)
        in
            (agrids, Array.fromList( List.concat [ elem , Array.toList aqueue ] ) )

    else
        (grids, Array.fromList [] )


{-| bfs algorithm. Direct means can't penetrate the valves.
queue is the array needed for the algorithm.
-}
bfsDirect : Grids -> Array Grid -> Int -> Int -> Grids
bfsDirect grids queue n m =
    if Array.length queue == 0 then grids
    else 
    let
        nqueue =
            List.map (checkSurround grids n m True) (Array.toList queue)
                |> List.concat
                |> Array.fromList

        (ngrids,nnqueue) =
            update grids nqueue
    in
    bfsDirect ngrids nnqueue n m

{-| bfs algorithm. Indirect means can penetrate the valves.
queue is the array needed for the algorithm.
-}
bfsIndirect : Grids -> Array Grid -> Int -> Int -> Grids
bfsIndirect grids queue n m =
    if Array.length queue == 0 then (grids)
    else 
    let
        nqueue =
            List.map (checkSurround grids n m False) (Array.toList queue)
                |> List.concat
                |> Array.fromList

        (ngrids,nnqueue) =
            update grids nqueue
    in
    bfsIndirect ngrids nnqueue n m

{-| Use the bfs algorithm to get every grid's distance to the exit.
Input the exit grid and the grids, return the grids with distance updated.
-}
bfs :  Grid -> Grids -> Grids
bfs exit grids =
    let

        n =
            Array.length grids
        m =
            Array.length (getrow 0 grids)
        nexit = 
            get grids exit.pos.x exit.pos.y
        ngrids = Array.map
                (\x ->
                    Array.map
                        (\y ->
                            if y.pos == nexit.pos then
                                Grid y.pos y.gridtype y.gstate (Just 0) True y.stype

                            else
                                Grid y.pos y.gridtype y.gstate Nothing False y.stype
                        )
                        x
                ) grids

        n2grids =
            bfsDirect ngrids (Array.fromList [ nexit ]) n m
        n3grids = Array.map
                (\x ->
                    Array.map
                        (\y ->
                            if y.pos == nexit.pos then
                                Grid y.pos y.gridtype y.gstate y.distance True y.stype

                            else
                                Grid y.pos y.gridtype y.gstate y.distance False y.stype
                        )
                        x
                ) n2grids
        n4grids = bfsIndirect n3grids (Array.fromList [ nexit ]) n m
    in
    n4grids
