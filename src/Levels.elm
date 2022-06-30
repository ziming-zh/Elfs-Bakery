module Levels exposing (..)
import Color exposing (..)
import Wall exposing (Wall)
import Valve exposing (Valve)
import Message exposing (Paint)
import Valve exposing (VState(..))
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
        ,[0,0,0,1,1,0,1,0]
        ,[1,0,0,0,1,0,1,0]
        ,[0,0,1,0,1,0,1,0]
        ,[0,1,1,0,1,0,1,0]
        ,[0,1,0,0,0,0,1,0]
        ,[1,1,1,1,1,1,0,1]]
        , col = mapInt2Bool
        [[1,1,0,1,1,1]
        ,[0,1,1,0,1,0]
        ,[1,0,1,1,1,0]
        ,[1,0,0,1,1,1]
        ,[1,1,0,0,0,0]
        ,[0,1,0,0,0,0]
        ,[1,1,0,0,0,1]
        ,[0,1,0,1,1,1]
        ,[1,1,1,1,1,1]]
    }
    ,valves = [
         {state = Up, pos = {x=3,y=1}}
        ,{state = Left, pos = {x=3,y=1}}
        ,{state = Left, pos = {x=3,y=2}}
        ,{state = Down, pos = {x=3,y=2}}
        ,{state = Up, pos = {x=4,y=3}}
        ,{state = Left, pos = {x=4,y=3}}
        ,{state = Up, pos = {x=6,y=4}}
        ,{state = Left, pos = {x=6,y=4}}
        ,{state = Down, pos = {x=1,y=3}}
        ,{state = Left, pos = {x=1,y=3}}
        ,{state = Down, pos = {x=7,y=0}}


    ]
    ,paints = [
         {pos = {x=2,y=0},color=Color.lightYellow}
        ,{pos = {x=2,y=2},color=Color.lightYellow}
        ,{pos = {x=3,y=0},color=Color.blue}
        ,{pos = {x=4,y=0},color=Color.blue}
        ,{pos = {x=5,y=0},color=Color.blue}
        ,{pos = {x=5,y=1},color=Color.blue}
        ,{pos = {x=5,y=2},color=Color.blue}
        ,{pos = {x=4,y=2},color=Color.blue}
        ,{pos = {x=0,y=2},color=Color.lightRed}
        ,{pos = {x=0,y=4},color=Color.lightRed}
        ,{pos = {x=0,y=6},color=Color.lightRed}
        ,{pos = {x=4,y=6},color=Color.lightRed}
    ]
    ,id = 1
    }



