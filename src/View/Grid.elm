module View.Grid exposing (..)
import Grid exposing (Grids,Grid)
import View.Basic exposing (rectRender)
import View.Basic exposing (setLength)
import Canvas exposing (Renderable)
import View.Basic exposing (setWidth)
import Color
import Array
import Grid exposing (GridType(..))


renderGrids : Grids -> List Renderable
renderGrids grids = 
    List.map (\y -> Array.toList (Array.map (\x-> drawGrid x) y)) (Array.toList grids)
    |> List.foldl List.append []


drawGrid : Grid -> Renderable
drawGrid grid =
    let
        posx = (toFloat grid.pos.x) * setLength
        posy = (toFloat grid.pos.y) * setLength
        color = 
            case grid.gridtype of
                Vacant -> Color.white
                Exit -> Color.lightGray
                Paint paint -> paint.color
    in
        rectRender posx posy (setLength) (setLength) color

        