
module Levels exposing (Level,initGuide,getInitialLevels)

{-| This library defines some variables for levels and draws
different levels
-}

import Color exposing (..)
import Wall exposing (Wall)
import Valve exposing (Valve)
import Message exposing (Paint,Pos,SpecialType(..),Stype,Sstate(..))
import Valve exposing (VState(..))
import Player exposing (Player,init)
type alias Level =
    { width : Int
    , height : Int
    , wall : Wall 
    , valves : List Valve
    , paints : List Paint
    , id : Int
    , exit : Pos
    , player : Player
    , colorseq : List Color.Color
    , stypes : List Stype
    }

getInitialLevels : List Level
getInitialLevels = [initLevel1,initLevel2,initLevel3,initLevel4,initLevel5,initLevel6,initLevel7]

initGuide : List Level
initGuide = [guideLevel1,guideLevel2,guideLevel3,guideLevel4]

mapInt2Bool : List (List Int) -> List (List Bool)
mapInt2Bool intlist = 
    let 
        i2b = (\a -> if a == 1 then True else False)
    in
        List.map (List.map i2b) intlist 

guideLevel4 : Level
guideLevel4 =
    { width = 7
    , height = 5
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,0,1,1,1,1]
        ,[0,0,0,0,1,1,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[1,1,1,0,1,1,0]
        ,[1,1,1,1,1,1,1]]
        , col = mapInt2Bool
        [[0,0,0,0,1]
        ,[0,0,0,0,0]
        ,[0,0,0,0,0]
        ,[1,1,1,1,0]
        ,[0,1,1,1,0]
        ,[0,0,0,0,0]
        ,[0,1,1,1,0]
        ,[1,1,1,1,1]]
    }
    ,valves = [
        {state = Left, pos = {y=4,x=1}}
       ,{state = Down, pos = {y=4,x=1}}
       ,{state = Right, pos = {y=6,x=2}}
       ,{state = Down, pos = {x=4,y=3}}
    ]
    ,paints = [
        {pos = {x=4,y=0},color=Color.red}
    ]
    ,id = 2
    ,exit = Pos 0 4
    ,colorseq = [Color.red]
    ,player = init (Pos 4 3) Message.Up
    ,stypes = [{pos ={x=1,y=3},state=Still 1,content=Chocolate,target=0}]
    }  

guideLevel3 : Level
guideLevel3 = 
    { width = 7
    , height = 3
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,1,0,1,0,0]
        ,[0,0,0,0,0,0,0]
        ,[1,1,0,1,0,1,1]
        ,[1,1,1,1,1,1,1]]
        , col = mapInt2Bool
        [[0,0,1]
        ,[0,0,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[0,0,0]
        ,[0,0,1]]
    }
    ,valves = [
        {state = Left, pos = {y=3,x=2}}
       ,{state = Right, pos = {y=4,x=2}}
    ]
    ,paints = [
        {pos = {x=0,y=2},color=Color.red}
       ,{pos = {x=0,y=4},color=Color.lightYellow}
    ]
    ,id = 2
    ,exit = Pos 2 6
    ,colorseq = [Color.orange]
    ,player = init (Pos 2 4) Message.Left
    ,stypes=[]
    }  

guideLevel2 : Level 
guideLevel2 = 
    { width = 7
    , height = 3
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,1,0,1,0,0]
        ,[0,0,0,0,0,0,0]
        ,[1,1,0,1,0,1,1]
        ,[1,1,1,1,1,1,1]]
        , col = mapInt2Bool
        [[0,0,1]
        ,[0,0,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[1,1,0]
        ,[0,0,0]
        ,[0,0,1]]
    }
    ,valves = [
        {state = Left, pos = {y=3,x=2}}
       ,{state = Right, pos = {y=4,x=2}}
    ]
    ,paints = [
        {pos = {x=0,y=2},color=Color.red}
       ,{pos = {x=0,y=4},color=Color.lightYellow}
    ]
    ,id = 2
    ,exit = Pos 2 6
    ,colorseq = [Color.red,Color.lightYellow]
    ,player = init (Pos 2 4) Message.Left
    ,stypes=[]
    }

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
    ,exit = Pos 0 5
    ,colorseq = [Color.lightYellow]
    ,player = init (Pos 0 0) Message.Right
    ,stypes=[]
    }

initLevel6 : Level
initLevel6 = 
    { width = 13
    , height = 13
    , wall = 
    {
        row =  mapInt2Bool
        [[1,1,1,1,1,1,1,1,1,1,1,1,1]
        ,[0,1,1,0,1,1,1,1,1,1,1,1,1]
        ,[0,0,1,1,0,1,1,1,1,1,1,0,0]
        ,[0,0,0,1,1,1,1,1,1,1,0,1,0]
        ,[0,0,0,0,1,1,0,1,1,0,1,0,0]
        ,[0,0,1,0,0,1,1,1,0,1,0,0,0]
        ,[0,0,0,0,0,0,0,1,1,0,0,0,0]
        ,[0,1,0,1,0,0,1,0,0,0,0,0,0]
        ,[1,0,1,0,0,1,0,1,0,0,0,0,0]
        ,[0,0,0,0,1,1,1,1,1,0,0,0,0]
        ,[0,0,0,1,1,1,1,1,1,1,0,0,0]
        ,[0,0,1,1,1,1,1,1,1,1,1,0,0]
        ,[0,1,1,1,1,1,1,1,1,1,1,0,0]
        ,[1,1,1,1,1,1,1,1,1,1,1,1,1]]
        , col = mapInt2Bool
        [[1,1,1,1,1,1,1,1,1,1,1,1,1]
        ,[0,1,1,1,1,1,1,0,1,1,1,1,0]
        ,[0,0,1,1,1,1,1,0,1,1,1,0,0]
        ,[0,0,0,1,1,0,1,1,1,1,0,0,0]
        ,[0,0,0,0,1,1,1,1,1,0,0,0,0]
        ,[0,0,0,0,0,1,1,1,0,0,0,0,0]
        ,[0,0,0,0,0,0,1,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,1,1,0,0,0,0]
        ,[0,0,0,1,1,0,1,0,0,0,0,0,0]
        ,[0,0,0,0,0,1,1,1,0,0,0,0,0]
        ,[0,0,0,0,1,0,1,1,1,1,0,0,0]
        ,[0,0,1,1,1,0,1,1,1,1,1,0,0]
        ,[1,1,0,1,1,1,1,1,1,1,1,1,0]
        ,[1,1,1,1,1,1,1,1,1,1,1,1,1]]
    }
    ,valves = [
         {state = Left , pos = {y=1,x=1}},{state = Up   , pos = {y=1,x=1}},{state = Down , pos = {y=1,x=1}},{state = Right, pos = {y=1,x=1}}
        ,{state = Left , pos = {y=12,x=12}},{state = Down   , pos = {y=12,x=12}}--,{state = Down , pos = {y=12,x=12}},{state = Right, pos = {y=12,x=12}}
        ,{state = Left , pos = {y=1,x=12}},{state = Down   , pos = {y=1,x=12}}--,{state = Down , pos = {y=1,x=12}},{state = Right, pos = {y=1,x=12}}
      --  ,{state = Left , pos = {y=12,x=1}},{state = Up   , pos = {y=12,x=1}},{state = Down , pos = {y=12,x=1}},{state = Right, pos = {y=12,x=1}}
        ,{state = Up , pos = {x=8,y=2}}--,{state = Right, pos = {x=8,y=2}},{state = Up   , pos = {x=8,y=2}}
        ,{state = Down , pos = {x=1,y=4}},{state = Left , pos = {x=1,y=4}}
        --,{state = Right , pos = {x=11,y=6}},{state = Down , pos = {x=11,y=6}},{state = Up , pos = {x=11,y=6}}--,{state = Left, pos = {x=11,y=6}}
        --,{state = Right , pos = {x=10,y=6}},{state = Down , pos = {x=10,y=6}},{state = Up , pos = {x=10,y=6}}--,{state = Left, pos = {x=10,y=6}}
        --,{state = Right , pos = {x=9,y=6}},{state = Down , pos = {x=9,y=6}},{state = Up , pos = {x=9,y=6}}--,{state = Left , pos = {x=9,y=6}}
        ,{state = Right , pos = {x=7,y=6}},{state = Down , pos = {x=7,y=6}},{state = Up , pos = {x=7,y=6}},{state = Left , pos = {x=7,y=6}}
        ,{state = Up , pos = {x=8,y=8}},{state = Right , pos = {x=8,y=8}}--,{state = Left , pos = {x=8,y=6}}        
        ,{state = Left , pos = {x=4,y=7}},{state = Up , pos = {x=4,y=7}}
        ,{state = Up , pos = {x=4,y=10}}
        ,{state = Left , pos = {x=2,y=5}}
        ,{state = Left , pos = {x=5,y=2}}
    ]
    ,paints = [
         {pos = {x=7,y=1},color=Color.lightYellow}
        ,{pos = {x=5,y=1},color=Color.blue}
        ,{pos = {x=6,y=1},color=Color.blue}
        ,{pos = {y=0,x=11},color=Color.red}
        ,{pos = {y=0,x=10},color=Color.red}
    ]
    ,id = 6
    ,exit = Pos 6 6
    ,stypes = [{pos ={x=7,y=6},state=Still 1,content=Vanilla,target=0}]
    ,colorseq = [Color.lightYellow, Color.purple]
    ,player = init (Pos 5 0) Message.Up
    }

initLevel1 : Level
initLevel1 = 
    { width = 7
    , height = 7
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,0,1,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[1,1,1,0,1,1,1]
        ,[1,1,1,0,1,1,1]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,1,0,0,0]]
        , col = mapInt2Bool
        [[0,0,0,1,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[1,1,1,0,1,1,1]
        ,[1,1,1,0,1,1,1]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,0,1,0,0,0]]
    }
    ,valves = [
         {state = Right, pos = {x=2,y=3}}
        ,{state = Down , pos = {x=3,y=2}}
        ,{state = Left , pos = {x=5,y=4}}
        ,{state = Up   , pos = {x=4,y=5}}
        ,{state = Left , pos = {x=3,y=4}}
        ,{state = Down , pos = {x=3,y=4}}
        ,{state = Up   , pos = {x=4,y=3}}
        ,{state = Right, pos = {x=4,y=3}}
    ]
    ,paints = [
         {pos = {x=0,y=3},color=Color.blue}
        ,{pos = {x=1,y=3},color=Color.blue}
        ,{pos = {x=3,y=0},color=Color.red}
        ,{pos = {x=3,y=1},color=Color.red}
        ,{pos = {x=6,y=3},color=Color.lightYellow}
        ,{pos = {x=5,y=3},color=Color.lightYellow}
    ]
    ,id = 1
    ,exit = Pos 3 6
    ,colorseq = [Color.green,Color.orange]
    ,player = init (Pos 3 3) Message.Down
    ,stypes=[]
    }

initLevel2 : Level
initLevel2 = 
    { width = 9
    , height = 9
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,0,0,1,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,1,0,1,0,0,0]
        ,[1,1,1,0,0,0,1,1,1]
        ,[1,1,1,0,0,0,1,1,1]
        ,[0,0,0,1,0,1,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,1,0,0,0,0]]
        , col = mapInt2Bool
        [[0,0,0,0,1,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,1,0,1,0,0,0]
        ,[1,1,1,0,0,0,1,1,1]
        ,[1,1,1,0,0,0,1,1,1]
        ,[0,0,0,1,0,1,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,1,0,0,0,0]]
    }
    ,valves = [
         {state = Left , pos = {x=3,y=5}}
        ,{state = Down , pos = {x=3,y=5}}
        ,{state = Right, pos = {x=4,y=3}}
        ,{state = Down , pos = {x=4,y=3}}
        ,{state = Up   , pos = {x=6,y=4}}
        ,{state = Right, pos = {x=6,y=4}}
        ,{state = Up   , pos = {x=5,y=6}}
        ,{state = Left , pos = {x=5,y=6}}
    ]
    ,paints = [
         {pos = {x=1,y=4},color=Color.blue}
        ,{pos = {x=2,y=4},color=Color.blue}
        ,{pos = {x=4,y=1},color=Color.red}
        ,{pos = {x=4,y=2},color=Color.red}
        ,{pos = {x=6,y=4},color=Color.lightYellow}
        ,{pos = {x=7,y=4},color=Color.lightYellow}
    ]
    ,id = 2
    ,exit = Pos 4 8
    ,colorseq = [Color.purple,Color.green,Color.orange]
    ,player = init (Pos 4 4) Message.Down
    ,stypes=[]
    }

initLevel5 : Level
initLevel5 = 
    { width = 11
    , height = 7
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,1,1,1,1,1,1,1,1,0]
        ,[1,0,0,1,1,1,1,1,0,0,0]
        ,[0,0,0,0,1,0,1,1,0,0,0]
        ,[0,1,0,0,0,0,0,1,1,0,1]
        ,[0,1,1,0,0,1,1,1,0,0,1]
        ,[1,0,0,0,0,1,0,1,1,0,0]
        ,[0,0,0,0,1,1,1,1,0,0,0]
        ,[0,0,0,1,1,1,1,1,1,1,0]]
        , col = mapInt2Bool
        [[0,1,1,1,1,0,0]
        ,[0,1,1,0,1,0,0]
        ,[1,1,1,0,0,0,0]
        ,[0,1,1,0,1,1,1]
        ,[0,0,1,1,1,1,0]
        ,[0,0,0,0,0,0,0]
        ,[0,0,1,1,0,1,0]
        ,[0,0,0,0,0,0,0]
        ,[0,1,0,0,0,0,0]
        ,[0,1,1,0,1,1,0]
        ,[1,1,1,0,1,1,1]
        ,[0,0,0,1,0,0,0]]
    }
    ,valves = [
         {pos = {x=2,y=10},state = Up},{pos = {x=2,y=10},state = Down},{pos = {x=2,y=10},state = Left},{pos = {x=2,y=10},state = Right}
        ,{pos = {x=5,y=10},state = Up},{pos = {x=5,y=10},state = Down},{pos = {x=5,y=10},state = Left},{pos = {x=5,y=10},state = Right}
        ,{pos = {x=3,y=1},state = Left}
        ,{pos = {x=4,y=0},state = Right}
        ,{pos = {x=3,y=3},state = Left}
        ,{pos = {x=3,y=3},state = Down}
        ,{pos = {x=4,y=8},state = Up}
        ,{pos = {x=4,y=8},state = Right}
        ,{pos = {x=5,y=5},state = Left}
    ]
    ,paints = [
         {pos = {x=1,y=0},color=Color.lightYellow}
        ,{pos = {x=2,y=0},color=Color.lightYellow}
        ,{pos = {x=5,y=4},color=Color.blue}
        ,{pos = {x=4,y=0},color=Color.red}
    ]
    ,id = 5
    ,exit = Pos 3 10
    ,stypes = [
         {pos ={x=1,y=9},state=Still 1,content=Vanilla,target=2}
        ,{pos ={x=5,y=9},state=Still 1,content=Chocolate,target=1}
    ]
    ,colorseq = [Color.green,Color.red,Color.lightYellow]

    ,player = init (Pos 3 1) Message.Right
    }

initLevel4 : Level
initLevel4 = 
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
        ,{state = Down, pos = {y=5,x=0}}
    ]
    ,paints = [
         {pos = {x=2,y=2},color=Color.lightYellow}
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
    ,id = 4
    ,exit = Pos 5 7
    ,stypes = [{pos ={x=2,y=4},state=Still 1,content=Vanilla,target=2}]
    ,colorseq = [Color.purple, Color.orange,Color.red]

    ,player = init (Pos 5 5) Message.Up
    }


initLevel3 : Level
initLevel3 = 
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
        -- {pos = {x=2,y=7},color=Color.lightYellow}
         {pos = {x=2,y=2},color=Color.lightYellow}
        ,{pos = {x=0,y=2},color=Color.red}
        --,{pos = {x=0,y=4},color=Color.red}
        ,{pos = {x=0,y=6},color=Color.blue}
        ,{pos = {x=4,y=6},color=Color.red}
    ]
    ,id = 3
    ,exit = Pos 5 7
    ,colorseq = [Color.orange, Color.purple]
    ,player = init (Pos 5 5) Message.Up
    ,stypes=[{pos ={x=4,y=4},state=Still 1,content=Chocolate,target=0}]
    }

initLevel7 : Level
initLevel7 = 
    { width = 16
    , height = 14
    , wall = 
    {
        row =  mapInt2Bool
        [[0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0]
        ,[1,1,1,1,0,1,1,1,1,0,0,0,1,1,0,0]
        ,[0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0]
        ,[0,0,0,0,0,1,1,0,1,1,1,0,0,0,1,1]
        ,[0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0]
        ,[0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0]
        ,[0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0]
        ,[0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
        -- ,[1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0]  
        ,[1,1,1,0,1,0,1,0,0,0,0,1,0,0,1,0]
        ,[0,0,0,0,1,1,1,0,0,0,1,0,1,1,1,1]
        ,[0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0]]
        , col = mapInt2Bool
        [[0,1,1,1,1,1,1,1,1,1,1,1,0,0]
        ,[0,0,1,1,1,1,1,1,1,1,1,0,0,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,1,1]
        ,[1,0,0,0,0,0,0,0,0,0,0,0,0,0]
        ,[0,0,1,0,1,1,0,1,1,1,1,1,0,0]
        ,[0,0,0,0,1,1,1,1,1,1,1,1,0,0]
        ,[0,0,1,0,0,0,0,0,0,0,0,0,0,0]
        ,[0,0,0,1,0,0,0,1,1,0,0,0,0,0]
        ,[0,0,0,1,0,0,0,0,0,0,0,0,0,0]
        ,[0,0,0,0,0,1,1,0,0,0,0,0,0,1]
        ,[0,0,0,1,0,0,0,0,0,0,0,0,1,0]
        ,[1,1,1,1,1,1,0,1,1,0,1,1,1,0]
        ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        ,[0,1,1,0,1,0,0,1,0,1,0,1,0,0]
        ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0]
        ,[0,0,0,1,1,1,1,1,1,1,1,1,1,0]
        ]
    }
    ,valves = [
         {state = Up, pos = {y=5,x=1}}
        ,{state = Down, pos = {y=5,x=1}}
        ,{state = Down, pos = {y=2,x=3}}
        ,{state = Right, pos = {y=2,x=3}}
        ,{state = Down, pos = {y=4,x=3}}
        ,{state = Left, pos = {y=4,x=3}}
        ,{state = Up, pos = {y=2,x=5}}
        ,{state = Right, pos = {y=2,x=5}}
        ,{state = Up, pos = {y=4,x=5}}
        ,{state = Left, pos = {y=4,x=5}}
        ,{state = Down, pos = {y=4,x=7}}
        ,{state = Up, pos = {y=4,x=7}}
        ,{state = Left, pos = {y=4,x=7}}
        ,{state = Down, pos = {y=7,x=14}}
        ,{state = Down, pos = {y=9,x=7}}
        ,{state = Right, pos = {y=9,x=7}}
        ,{state = Right, pos = {y=10,x=2}} 
        ,{state = Right, pos = {y=10,x=4}} 
        ,{state = Left, pos = {y=10,x=4}}
        ,{state = Up, pos = {y=13,x=4}}  
        ,{state = Down, pos = {y=13,x=4}}
        ,{state = Up, pos = {y=13,x=6}}  
        ,{state = Down, pos = {y=13,x=6}}
        ,{state = Up, pos = {y=14,x=6}}  
        ,{state = Right, pos = {y=14,x=6}}
        ,{state = Down, pos = {y=15,x=5}}  
        ,{state = Up, pos = {y=15,x=8}}  
        ,{state = Right, pos = {y=15,x=8}}  
        ,{state = Down, pos = {y=15,x=9}}  
        ,{state = Up, pos = {y=15,x=12}}
        ,{state = Right, pos = {y=11,x=8}} 
        ,{state = Down, pos = {y=11,x=8}}
        ,{state = Down, pos = {y=12,x=9}}
        ,{state = Down, pos = {y=7,x=12}}    
    ]
    ,paints = [
         {pos = {x=5,y=14},color=Color.lightYellow}
        ,{pos = {x=7,y=14},color=Color.lightYellow}
        ,{pos = {x=9,y=14},color=Color.lightYellow}
        ,{pos = {x=11,y=14},color=Color.lightYellow}
        ,{pos = {x=8,y=11},color=Color.lightYellow}
        ,{pos = {x=14,y=4},color=Color.lightYellow}
        ,{pos = {x=0,y=5},color=Color.blue}
        ,{pos = {x=0,y=6},color=Color.blue}
        ,{pos = {x=0,y=7},color=Color.blue}
        ,{pos = {x=1,y=5},color=Color.blue}
        ,{pos = {x=1,y=6},color=Color.blue}
        ,{pos = {x=1,y=7},color=Color.blue}
        ,{pos = {x=3,y=2},color=Color.red}
        ,{pos = {x=3,y=3},color=Color.red}
        ,{pos = {x=3,y=9},color=Color.red}
        ,{pos = {x=3,y=10},color=Color.red}
        ,{pos = {x=4,y=2},color=Color.red}
        ,{pos = {x=4,y=3},color=Color.red}
    ]
    ,id = 7
    ,exit = Pos 3 12
    ,colorseq = [Color.lightYellow, Color.purple,Color.green,Color.lightYellow, Color.purple,Color.green]
    ,player = init (Pos 0 11) Message.Down
    ,stypes=[{pos ={x=7,y=5},state=Still 1,content=Chocolate,target=1}]
    }
