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
        r = 255-(Maybe.withDefault 0 grid.distance) * 2
        g = 234-(Maybe.withDefault 0 grid.distance) * 6
        b = 236-(Maybe.withDefault 0 grid.distance) * 3
        color = 
            case grid.gridtype of
                Vacant -> Color.rgb255 r g b 
                Exit -> Color.lightGray
                Paint paint -> paint.color
    in
        rectRender posx posy (setLength) (setLength) color

        