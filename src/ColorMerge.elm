module ColorMerge exposing (mergeColor)
{-| merge color based on three color principle

# Function
@docs mergeColor

-}
import Color exposing (..)
mergeRed : Color -> Color
mergeRed color =
    if (color == red) then
        Color.red
    else if (color == blue ) then
        Color.purple
    else if (color == Color.lightYellow) then
        orange
    else if (color == Color.orange ) then
        Color.orange
    else if (color == Color.green) then
        white
    else if (color == purple ) then
        Color.purple
    else 
        white
mergeYellow : Color -> Color
mergeYellow color =
    if (color == red) then
        Color.orange
    else if (color == blue ) then
        Color.green
    else if (color == Color.lightYellow) then
        lightYellow
    else if (color == Color.orange ) then
        Color.orange
    else if (color == Color.green) then
        green
    else if (color == purple ) then
        white
    else 
        white
mergeBlue : Color -> Color
mergeBlue color =
    if (color == red) then
        purple
    else if (color == blue ) then
        Color.blue
    else if (color == Color.lightYellow) then
        green
    else if (color == Color.orange ) then
        white
    else if (color == Color.green) then
        green
    else if (color == purple ) then
        purple
    else 
        white
mergeOrange : Color -> Color
mergeOrange color =
    if (color == red) then
        Color.orange
    else if (color == blue ) then
        Color.white
    else if (color == Color.lightYellow) then
        orange
    else if (color == Color.orange ) then
        Color.orange
    else if (color == Color.green) then
        white
    else if (color == purple ) then
        white
    else 
        white
mergePurple : Color -> Color
mergePurple color =
    if (color == red) then
        Color.purple
    else if (color == blue ) then
        Color.purple
    else if (color == Color.lightYellow) then
        white
    else if (color == Color.orange ) then
        white
    else if (color == Color.green) then
        white
    else if (color == purple ) then
        Color.purple
    else 
        white
mergeGreen : Color -> Color
mergeGreen color =
    if (color == red) then
        white
    else if (color == blue ) then
        green
    else if (color == Color.lightYellow) then
        green
    else if (color == Color.orange ) then
        white
    else if (color == Color.green) then
        green
    else if (color == purple ) then
        white
    else 
        white
{-| merge color based on three color principle
-}

mergeColor : Color -> Color -> Color
mergeColor a b =
    if a == b then
        a

    else if a == white || b == white then
        white

    else if a==red then
        mergeRed b
    else if a==blue then
        mergeBlue b
    else if a==lightYellow then
        mergeYellow b
    else if a==orange then
        mergeOrange b
    else if a==green then
        mergeGreen b
    else if a==purple then
        mergePurple b
    else white