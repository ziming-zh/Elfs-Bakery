module Color exposing (Color)

toString : RGBColor -> String
toString rgbcolor =
    case rgbcolor of 
    {red,green,blue} ->
        "rgb("
            ++ String.fromInt red
            ++ ","
            ++ String.fromInt green
            ++ ","
            ++ String.fromInt blue
            ++ ")"

type Color
    = SpecialColor
    | Normal NormalColor

type NormalColor
    = Blue
    | Yellow
    | Purple
    | Black
    | Grey
    | Noth
    | Nocolor

type2color : Color -> String
type2color color = 
    case color of 
        Normal normalcolor -> 
            case normalcolor of 
                Blue -> "#33CCFF"
                Yellow -> "#FFB266"
                Purple -> "#3380BC"
                Grey -> "#CCCCCC"
                Black -> "#646464"
                Nocolor -> "#FFFFFF"
                Noth -> "#ECF0F1"
        _ -> "#FF6666"


type alias RGBColor
    =  { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> RGBColor
rgb red green blue =
   { red = red, green = green, blue = blue }

