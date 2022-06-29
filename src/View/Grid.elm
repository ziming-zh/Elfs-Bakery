module View.Grid exposing (..)
import Grid exposing (Grid)
import View.Basic exposing (rectRender)
import View.Basic exposing (setLength)
import Canvas exposing (Renderable)
import View.Basic exposing (setWidth)


renderGrids : List Grid -> List Renderable
renderGrids grids = 
    List.map drawGrid grids


drawGrid : Grid -> Renderable
drawGrid grid =
    let
        posx = (toFloat grid.pos.x) * setLength +setWidth

        posy = (toFloat grid.pos.y) * setLength +setWidth
        
        color = grid.color
    in
        rectRender posx posy (setLength-setWidth*2) (setLength-setWidth*2) color

        