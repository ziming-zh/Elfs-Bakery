module View.Hat exposing (hat)


import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)


hat : Int -> Html.Attribute msg
hat i =
        HtmlAttr.src ("./assets/hat/hat"++ String.fromInt i++".png")