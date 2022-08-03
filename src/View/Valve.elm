module View.Valve exposing (renderValves)
{-| draw valves

# Function
@docs renderValves

-}
import Valve exposing (VState(..),Valve)
import Canvas exposing (Renderable)
import View.Basic exposing (circleRender,setLength,setWidth)
import Color exposing (Color)
import View.Basic exposing (rectRender)

{-| draw valves
-}
renderValves : List Valve -> List Renderable
renderValves valves = 
    List.foldl List.append [] (List.map drawValve valves)

drawValve : Valve -> List Renderable
drawValve valve =
    let
        posx = (toFloat valve.pos.y) * setLength

        posy = (toFloat valve.pos.x) * setLength
        state = valve.state
    in
        [circleRender posx posy setWidth Color.lightPurple] ++
        case state of 
            Up -> [rectRender (posx-setWidth/4) (posy-setLength) setWidth setLength Color.lightPurple]
            Down -> [rectRender (posx-setWidth/4) posy setWidth setLength Color.lightPurple] 
            Left -> [rectRender (posx-setLength) (posy-setWidth/4) setLength setWidth Color.lightPurple]
            Right -> [rectRender posx (posy-setWidth/4) setLength setWidth Color.lightPurple]
