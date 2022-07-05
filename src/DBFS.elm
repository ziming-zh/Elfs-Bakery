module DBFS exposing (bfs)

import Array exposing (Array)
import Grid exposing (Grid, Grids, IsOpen(..), initGrid)
import Html exposing (a)
import Html.Attributes exposing (list)
import Set exposing (Set)



-- all the grids, can update others


type Bfstype
    = Direct
    | Indirect


gett : Int -> Array Grid -> Grid
gett x grids =
    case Array.get x grids of
        Just a ->
            a

        Nothing ->
            initGrid -1 -1

getBool : Int -> List Bool -> Bool
getBool i list = 
    let
        n = List.length list
    in
    case ( List.head (List.drop (n-i+1) list) ) of
        Just a -> a
        Nothing -> False
        

setBool : Int -> List Bool -> List Bool
setBool i list = 
    let
        n = List.length list
    in
        List.concat[ List.take (i-1) list , [True] , List.drop (n-i) list ]

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


checkSurround : List Bool -> Grids -> Int -> Int -> Bool -> Grid -> List Grid
checkSurround vis grids n m tp grid =
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
                [ if x > 0 && check grid.gstate.up && (getBool ((x-1)*m+y) vis) then
                    [ get grids (x - 1) y ]

                  else
                    []
                , if x < (n - 1) && check grid.gstate.down && (getBool ((x+1)*m+y) vis) then
                    [ get grids (x + 1) y ]

                  else
                    []
                , if y > 0 && check grid.gstate.left && (getBool (x*m+y-1) vis) then
                    [ get grids x (y - 1) ]

                  else
                    []
                , if y < (m - 1) && check grid.gstate.right && (getBool (x*m+y+1) vis) then
                    [ get grids x (y + 1) ]

                  else
                    []
                ]

        nnqueue = List.filter (\xx -> xx.distance == Nothing) nqueue

        nnnqueue =
            List.map (\xx -> Grid xx.pos xx.gridtype xx.gstate (Just distance) xx.renewed) nnqueue
    in
    nnnqueue


update : List Bool -> Grids -> Array Grid -> (Grids,List Bool)
update vis grids queue =
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

            (ngrids,nvis) =
                if getBool (x*m+y) vis then 
                    (grids,vis)
                else
                    (Array.set x (Array.set y grid (getrow x grids)) grids,setBool (x*m+y) vis)
        in
        update nvis ngrids (Array.slice 1 (Array.length queue) queue)

    else
        (grids,vis)


bfsDirect : List Bool -> Grids -> Array Grid -> Int -> Int -> Grids
bfsDirect vis grids queue n m =
    if Array.length queue == 0 then (grids)
    else 
    let
        nqueue =
            List.map (checkSurround vis grids n m True) (Array.toList queue)
                |> List.concat
                |> Array.fromList

        (ngrids,nvis) =
            update vis grids nqueue
    in
    bfsDirect nvis ngrids nqueue n m

bfsIndirect : List Bool -> Grids -> Array Grid -> Int -> Int -> Grids
bfsIndirect vis grids queue n m =
    if Array.length queue == 0 then (grids)
    else 
    let
        nqueue =
            List.map (checkSurround vis grids n m False) (Array.toList queue)
                |> List.concat
                |> Array.fromList

        (ngrids,nvis) =
            update vis grids nqueue
    in
    bfsDirect nvis ngrids nqueue n m

bfs :  Grid -> Grids -> Grids
bfs exit grids =
    let
        gridline =
            getrow 0 grids

        n =
            Array.length grids
        m =
            Array.length (getrow 0 grids)
        vis = List.repeat (n*m) False
        nvis = setBool (exit.pos.x*m+exit.pos.y) vis
        ngrids =
            Array.map
                (\x ->
                    Array.map
                        (\y ->
                            if y.pos == exit.pos then
                                Grid y.pos y.gridtype y.gstate (Just 0) y.renewed

                            else
                                Grid y.pos y.gridtype y.gstate Nothing y.renewed
                        )
                        x
                )
                grids

        nngrids =
            bfsDirect nvis ngrids (Array.fromList [ exit ]) n m
        --nnngrids = 
          --  bfsIndirect nnvis nngrids (Array.fromList [ exit ]) n m
    in
    nngrids