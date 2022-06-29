module Levels exposing (..)
import Color exposing (..)
type alias Level =
    { width : Int
    , height : Int
    , map : List (List Char) --- revision express the map by characters rather than by types -- shrink spaces
    --- the settings are all squeezed into the map property
    , colorseq : List Color
    , id : String
    }


type alias EncodeLevel = 
    String




