module DBFS exposing (bfs,get)

import Array exposing (Array)
import Grid exposing (Grid, Grids, IsOpen(..), initGrid)
import Html exposing (a)
import Html.Attributes exposing (list)
import Set exposing (Set)
import Svg.Attributes exposing (y)



-- all the grids, can update others


gett : Int -> Array Grid -> Grid
gett x grids =
    case Array.get x grids of
        Just a ->
            a

        Nothing ->
            initGrid -1 -1

getBool : (Int,Int) -> Grids -> Bool
getBool (x,y) grids = 
    let
        grid = get grids x y
    in
        grid.renewed
        

setBool : (Int,Int) -> Grids -> Grids
setBool (x,y) grids = 
    let
        grid = get grids x y
        ngrid = Grid grid.pos grid.gridtype grid.gstate grid.distance True
    in
        Array.set x ( Array.set y ngrid (getrow x grids) ) grids

getrow : Int -> Grids -> Array Grid
getrow x grids =
    case Array.get x grids of
        Just a ->
            a

        Nothing ->
            Array.fromList [ initGrid -1 -1 ]


get : Grids -> Int -> Int -> Grid
get grids x y =
    getrow x grids
        |> gett y


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
                \xx ->
                case xx.distance of
                    Just a -> Grid xx.pos xx.gridtype xx.gstate (Just a) True
                    Nothing -> Grid xx.pos xx.gridtype xx.gstate (Just distance) True
            ) nqueue
    in
    nnnqueue


update :Grids -> Array Grid -> (Grids,Array Grid)
update grids queue =
  --  if Array.length queue > 0 then
    if Array.length queue > 0 then
        let
            m =
                Array.length (getrow 0 grids)
            grid =
                gett 0 queue

            x =
                grid.pos.x

            y =
                grid.pos.y

            ngrids =
                if getBool (x,y) grids then 
                    grids
                else
                    Array.set x (Array.set y grid (getrow x grids)) grids
            (agrids,aqueue) = 
                update ngrids (Array.slice 1 (Array.length queue) queue)
        in
            (agrids, Array.fromList( List.concat [ [grid] , Array.toList aqueue ] ) )

    else
        (grids, Array.fromList [] )


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

bfs :  Grid -> Grids -> Grids
bfs exit grids =
    let

        n =
            Array.length grids
        m =
            Array.length (getrow 0 grids)
        ngrids = Array.map
                (\x ->
                    Array.map
                        (\y ->
                            if y.pos == exit.pos then
                                Grid y.pos y.gridtype y.gstate (Just 0) True

                            else
                                Grid y.pos y.gridtype y.gstate Nothing False
                        )
                        x
                ) grids

        n2grids =
            bfsDirect ngrids (Array.fromList [ exit ]) n m
        n3grids = Array.map
                (\x ->
                    Array.map
                        (\y ->
                            if y.pos == exit.pos then
                                Grid y.pos y.gridtype y.gstate y.distance True

                            else
                                Grid y.pos y.gridtype y.gstate y.distance False
                        )
                        x
                ) n2grids
        n4grids = bfsIndirect n3grids (Array.fromList [ exit ]) n m
    in
    n4grids