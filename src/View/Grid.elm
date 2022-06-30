module View.Grid exposing (..)
import Grid exposing (Grids,Grid)
import View.Basic exposing (rectRender)
import View.Basic exposing (setLength)
import Canvas exposing (Renderable)
import View.Basic exposing (setWidth)
import Color
import Array


renderGrids : Grids -> List Renderable
renderGrids grids = 
    List.map (\y -> Array.toList (Array.map (\x-> drawGrid x) y)) (Array.toList grids)
    |> List.foldl List.append []


drawGrid : Grid -> Renderable
drawGrid grid =
    let
        posx = (toFloat grid.pos.x-1) * setLength +setWidth

        posy = (toFloat grid.pos.y-1) * setLength +setWidth
        
        color = 
            case grid.paint of
                Nothing -> Color.white
                Just paint -> paint.color
    in
        rectRender posx posy (setLength-setWidth) (setLength-setWidth) color

        