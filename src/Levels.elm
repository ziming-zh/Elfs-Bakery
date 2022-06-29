module Levels exposing (..)
import Color exposing (..)
import Wall exposing (Wall)
import Valve exposing (Valve)
import Message exposing (Paint)
type alias Level =
    { width : Int
    , height : Int
    , wall : Wall --- revision express the map by characters rather than by types -- shrink spaces
    --- the settings are all squeezed into the map property
    , valves : List Valve
    , paints : List Paint
    , id : Int
    }

getInitialLevels: List Level
getInitialLevels = [initLevel1]

mapInt2Bool : List (List Int) -> List (List Bool)
mapInt2Bool intlist = 
    let 
        i2b = (\a -> if a == 1 then True else False)
    in
        List.map (List.map i2b) intlist 

initLevel1 : Level
initLevel1 = 
    { width = 8
    , height = 6
    , wall = 
    {
        row =  mapInt2Bool
        [[1,1,1,0,1,1,1,1]
        ,[0,0,1,1,1,0,1,0]
        ,[1,0,0,0,1,0,1,0]
        ,[0,0,1,0,1,0,1,0]
        ,[0,1,1,0,1,1,1,0]
        ,[0,1,0,0,0,0,1,0]
        ,[1,1,1,1,1,1,0,1]]
        , col = mapInt2Bool
        [[1,1,0,1,1,1]
        ,[0,1,1,1,1,0]
        ,[1,0,1,1,1,0]
        ,[1,0,1,1,1,1]
        ,[1,1,1,0,0,0]
        ,[0,1,0,0,0,0]
        ,[1,1,0,1,0,1]
        ,[1,1,0,1,1,1]
        ,[1,1,1,1,1,1]]
    }
    ,valves = []
    ,paints = []
    ,id = 1
    }



