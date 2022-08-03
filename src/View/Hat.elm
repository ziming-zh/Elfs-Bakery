module View.Hat exposing (hat)
{-| draw the ith hat

# Function
@docs hat

-}
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)

{-| draw the ith hat
-}

hat : Int -> Html.Attribute msg
hat i =
        HtmlAttr.src ("./assets/hat/hat"++ String.fromInt i++".png")