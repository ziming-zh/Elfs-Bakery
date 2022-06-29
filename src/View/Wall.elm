module View.Wall exposing (..)
import View.Basic exposing (rectRender,setLength,setWidth)
import Wall exposing (Wall)
import Canvas exposing (Renderable)
import Color exposing (Color)
import List exposing (indexedMap)



allignRow : Int -> Int -> Bool -> Renderable
allignRow idy idx exists =
    if exists == True then
        rectRender ((toFloat idx)*setLength) ((toFloat idy)*setLength) setLength setWidth Color.black
    else 
        rectRender 0 0 0 0 Color.black

allignCol : Int -> Int -> Bool -> Renderable
allignCol idy idx exists =
    if exists == True then
        rectRender ((toFloat idx)*setLength) ((toFloat idy)*setLength) setWidth setLength Color.black
    else 
        rectRender 0 0 0 0 Color.black

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

