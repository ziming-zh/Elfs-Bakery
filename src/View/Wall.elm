module View.Wall exposing (drawWall)
import View.Basic exposing (rectRender,setLength,setWidth)
import Wall exposing (Wall)
import Canvas exposing (Renderable)
import Color exposing (Color)
import Color exposing (rgb255)


defaultWallColor : Color
defaultWallColor = rgb255 219 149 99

allignRow : Int -> Int -> Bool -> Renderable
allignRow idy idx exists =
    if exists == True then
        rectRender ((toFloat idx)*setLength) ((toFloat idy)*setLength) setLength setWidth defaultWallColor
    else 
        rectRender 0 0 0 0 defaultWallColor

allignCol : Int -> Int -> Bool -> Renderable
allignCol idy idx exists =
    if exists == True then
        rectRender ((toFloat idy)*setLength) ((toFloat idx)*setLength) setWidth setLength defaultWallColor
    else 
        rectRender 0 0 0 0 defaultWallColor


drawWall : Wall -> List Renderable
drawWall wall =
    let
        row = wall.row
        col = wall.col
        
    in
        
        List.append
            (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (allignRow a)) row))
            (List.foldl List.append [] (List.indexedMap (\a -> List.indexedMap (allignCol a)) col))
        
        ---List List Bool -> List Bool -> Bool with index 
        -- indexedMap (Int-> a -> b) List a -> List b
    --row

    --column

