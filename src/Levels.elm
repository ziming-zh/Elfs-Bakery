module Levels exposing (..)
import Color exposing (..)
import Wall exposing (Wall)
import Valve exposing (Valve)
import Message exposing (Paint)
import Valve exposing (VState(..))
import Message exposing (Pos)
type alias Level =
    { width : Int
    , height : Int
    , wall : Wall --- revision express the map by characters rather than by types -- shrink spaces
    --- the settings are all squeezed into the map property
    , valves : List Valve
    , paints : List Paint
    , id : Int
    , exit : Pos
    , colorseq : List Color.Color
    }

getInitialLevels : List Level
getInitialLevels = [initLevel1]

initGuide : List Level
initGuide = [guideLevel1]

mapInt2Bool : List (List Int) -> List (List Bool)
mapInt2Bool intlist = 
    let 
        i2b = (\a -> if a == 1 then True else False)
    in
        List.map (List.map i2b) intlist 

guideLevel1 : Level 
guideLevel1 = 
    { width = 6
    , height = 1
    , wall = 
    {
        row =  mapInt2Bool
        [[1,1,1,1,1,1]
        ,[1,1,1,1,1,1]]
        , col = mapInt2Bool
        [[1]
        ,[0]
        ,[0]
        ,[0]
        ,[0]
        ,[0]
        ,[1]]
    }
    ,valves = [
        {state = Down, pos = {y=3,x=0}}
    ]
    ,paints = [
        {pos = {x=0,y=0},color=Color.lightYellow}
    ]
    ,id = 1
    ,exit = Pos 5 7
    ,colorseq = [Color.red , Color.blue, Color.lightYellow]
    }


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
        [[1,1,1,1,1,1]
        ,[0,1,0,0,1,0]
        ,[1,0,1,1,1,0]
        ,[1,0,0,1,1,1]
        ,[1,1,0,0,0,0]
        ,[0,1,0,0,0,0]
        ,[1,1,0,0,0,1]
        ,[0,1,0,1,1,1]
        ,[1,1,1,1,1,1]]
    }
    ,valves = [
         {state = Left, pos = {y=3,x=1}}
        ,{state = Left, pos = {y=3,x=2}}
        ,{state = Down, pos = {y=3,x=2}}
        ,{state = Up, pos = {y=4,x=3}}
        ,{state = Left, pos = {y=4,x=3}}
        ,{state = Up, pos = {y=6,x=4}}
        ,{state = Down, pos = {y=6,x=4}}
        ,{state = Down, pos = {y=5,x=4}}
        ,{state = Down, pos = {y=4,x=4}}
        ,{state = Down, pos = {y=1,x=3}}
        ,{state = Left, pos = {y=1,x=3}}
        ,{state = Down, pos = {y=7,x=0}}
    ]
    ,paints = [
         {pos = {x=2,y=7},color=Color.lightYellow}
        ,{pos = {x=2,y=2},color=Color.lightYellow}
        ,{pos = {x=3,y=0},color=Color.blue}
        ,{pos = {x=4,y=0},color=Color.blue}
        ,{pos = {x=5,y=0},color=Color.blue}
        ,{pos = {x=5,y=1},color=Color.blue}
        ,{pos = {x=5,y=2},color=Color.blue}
        ,{pos = {x=4,y=2},color=Color.blue}
        ,{pos = {x=0,y=2},color=Color.red}
        ,{pos = {x=0,y=4},color=Color.red}
        ,{pos = {x=0,y=6},color=Color.red}
        ,{pos = {x=4,y=6},color=Color.red}
    ]
    ,id = 1
    ,exit = Pos 5 7
    ,colorseq = [Color.red , Color.blue, Color.lightYellow]
    }



