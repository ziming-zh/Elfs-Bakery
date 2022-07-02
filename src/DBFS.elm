module DBFS exposing (bfs)
import Html exposing (a)
import Array exposing (Array)
import Grid exposing (Grids,Grid,IsOpen(..),initGrid)
import Message exposing (MoveDirection)

-- all the grids, can update others

type Bfstype 
    = Direct 
    | Indirect

gett : Int -> Array Grid -> Grid
gett x grids =
    case (Array.get x grids) of
        Just a -> a
        Nothing -> initGrid -1 -1

getrow : Int -> Grids -> Array Grid
getrow x grids =
    case (Array.get x grids) of
        Just a -> a
        Nothing -> Array.fromList [initGrid -1 -1]

get : Grids -> Int -> Int -> Grid
get grids x y =
    getrow x grids 
        |> gett y

checkSurround : Grids -> Int -> Int -> Grid -> List Grid
checkSurround grids n m grid =
    let
        x = grid.pos.x
        y = grid.pos.y
        nqueue = 
            (List.concat
                [   if x>0 && grid.gstate.up == Open then [get grids (x-1) y]
                    else []
                ,   if x<(n-1) && grid.gstate.down == Open then [get grids (x+1) y]
                    else []
                ,   if y>0 && grid.gstate.left == Open then [get grids x (y-1)]
                    else []
                ,   if y<(m-1) && grid.gstate.right == Open then [get grids x (y+1)]
                    else []
                ])
        nnqueue = List.map (\xx -> (Grid xx.pos xx.color xx.gstate (grid.dis+1))) nqueue
    in
        nnqueue

update : Grids -> Array Grid -> Grids
update grids queue =
    if Array.length queue > 0 then
        let
            grid = gett 0 queue
            x = grid.pos.x
            y = grid.pos.y
            ngrids = Array.set x ( Array.set y grid ( getrow x grids ) ) grids

        in
            update ngrids (Array.slice 1 (Array.length queue) queue)
    else
        grids

bfsdirect : Grids -> Array Grid -> Int -> Int -> Grids
bfsdirect grids queue n m =

    let
        nqueue = List.map (checkSurround grids n m) (Array.toList queue)
                    |> List.concat
                    |> Array.fromList
        ngrids = update grids nqueue
    in
        bfsdirect ngrids nqueue n m

bfs : Grids -> Grid -> Grids
bfs grids exit = 
    let 
        gridline = getrow 0 grids
        n = Array.length grids
        m = Array.length (getrow 0 grids)
        ngrids = Array.map ( \x -> ( Array.map ( \y -> (Grid y.pos y.color y.gstate 1000) ) x )  ) grids
        nngrids = bfsdirect ngrids (Array.fromList [exit]) n m
    in
        ngrids


