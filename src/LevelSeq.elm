{- The Level Seq type is a list of Levels that could easily be appended or prepended
, which in a way makes our game design more flexible
-}
module LevelSeq
    exposing
        ( getInitialLevels
        , getLevel
        , appendLevel
        , prependLevel
        , removeLevel
        , isDuplicate
        , getIndexOf
        , LevelSeq
        )
import Set
import Array exposing (Array)
import Dict

import Levels exposing (Level,EncodeLevel)



type alias LevelSeq =
    Array EncodeLevel

type alias ViewLevel = Level -- TO BE ADJUSTED

-- getViewLevelFromEncodedLevel : EncodeLevel -> ViewLevel
-- getViewLevelFromEncodedLevel encodedLevel = 0


getIndexOf : EncodeLevel -> LevelSeq -> Int
getIndexOf levelId levels =
    levels
        |> Array.toIndexedList
        |> List.map (\( index, id ) -> ( id, index ))
        |> Dict.fromList
        |> Dict.get levelId
        |> Maybe.withDefault -1


getLevel : Int -> LevelSeq -> EncodeLevel
getLevel levelIndex levels =
    levels
        |> Array.get levelIndex
        |> Maybe.withDefault ""


isDuplicate : EncodeLevel -> LevelSeq -> Bool
isDuplicate levelId levels =
    levels
        |> Array.filter (\level -> level == levelId)
        |> Array.isEmpty
        |> not


appendLevel : EncodeLevel -> LevelSeq -> LevelSeq
appendLevel levelId levels =
    if (isDuplicate levelId levels) then
        levels
    else
        Array.push levelId levels


prependLevel : EncodeLevel -> LevelSeq -> LevelSeq
prependLevel levelId levels =
    if (isDuplicate levelId levels) then
        levels
    else
        Array.toList levels
            |> (::) levelId
            |> Array.fromList


removeLevel : EncodeLevel -> LevelSeq -> LevelSeq
removeLevel levelId levels =
    levels
        |> Array.filter (\level -> not (level == levelId))


getInitialLevels : LevelSeq
getInitialLevels =
    let
        stringLevels =
            [ stringLevel0
            , stringLevel1
            , stringLevel2
            , stringLevel3
            ]
    in
        stringLevels
            -- |> List.filterMap getLevelFromString
            -- |> List.map .id
            |> Array.fromList
detectLevelChar : Char -> Bool
detectLevelChar char =
    let
        allowedChars =
            [ '#', 'r', 'g', 'b', 'p', 'y', 'o', 'w', '@', '+'  ,' ' ]
    in
        Set.member char (Set.fromList allowedChars)

stringLevel0 : String
stringLevel0 =
    """
#############
#           #
#     r     #
#           #
#     @     #
#           #
#############
"""
stringLevel1 : String
stringLevel1 =
    """
#############
#           #
#     r     #
#           #
#     @     #
#           #
#############
"""

stringLevel2 : String
stringLevel2 =
    """
#############
#           #
#     r     #
#           #
#     @     #
#           #
#############
"""

stringLevel3 : String
stringLevel3 =
    """
#############
#           #
#     r     #
#           #
#     @     #
#           #
#############
"""


--- Design the meaning of different symbols and attach the meanings here
{-
# -> wall
r -> Red Stream
b -> Blue Stream
g -> Grey Stream
+ -> Valve
@ -> Character

-}
