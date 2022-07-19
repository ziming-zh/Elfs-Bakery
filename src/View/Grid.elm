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
    let
        listgrid =  Array.toList ( Array.map ( \x -> Array.toList x ) grids )
        listgrids = List.concat listgrid
        listdis = List.map  (
                            \x -> 
                                case x.distance of 
                                    Nothing -> 0 
                                    Just a -> a
                            ) listgrids
        mx = Basics.max (List.foldl Basics.max 0 listdis) 1
    in
        List.map (\y -> Array.toList (Array.map (\x-> drawGrid (Basics.toFloat mx) x) y)) (Array.toList grids)
        |> List.foldl List.append []


drawGrid : Float -> Grid -> Renderable
drawGrid mx grid =
    let
        posy = (toFloat grid.pos.x) * setLength
        posx = (toFloat grid.pos.y) * setLength
        r = (254-(Basics.toFloat (Maybe.withDefault 0 grid.distance))/mx * 20)/255
        g = (248-(Basics.toFloat (Maybe.withDefault 0 grid.distance))/mx * 80)/255
        b = (231-(Basics.toFloat(Maybe.withDefault 0 grid.distance))/mx * 20)/255
        color = 
            case grid.gridtype of
                Vacant -> Color.rgb r g b 
                Exit -> Color.lightGray
                Paint paint -> paint.color
    in
        rectRender posx posy (setLength) (setLength) color

        