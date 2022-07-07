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
        posy = (toFloat grid.pos.x) * setLength
        posx = (toFloat grid.pos.y) * setLength
        d = 255-(Maybe.withDefault 0 grid.distance) * 5
        color = 
            case grid.gridtype of
                Vacant -> Color.rgb255 d d d 
                Exit -> Color.lightGray
                Paint paint -> paint.color
    in
        rectRender posx posy (setLength) (setLength) color

        