module View.Level exposing (..)
import Canvas exposing (Renderable)
import View.Grid exposing (renderGrids)
import Valve exposing (Valve)
import Wall exposing (Wall)
import Grid exposing (Grid)
import View.Wall exposing (drawWall)
import View.Valve exposing (renderValves)
import Grid exposing (Grids)
import Player exposing (Player)
import View.Player exposing (renderPlayer)

renderLevel : Wall -> List Valve -> Grids -> Player -> List Renderable
renderLevel wall valves grids player =
    (renderGrids grids) ++
    (drawWall wall) ++
    (renderValves valves) ++
    [renderPlayer player]
