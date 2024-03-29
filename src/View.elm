module View exposing (view)

{-| This library combines all the view functions.

# Functions
@docs view

-}

import Model exposing (Model)
import Message exposing (Msg(..))
import Html exposing (Html, div)
import View.Level exposing (renderLevelPage)
import View.Home exposing(renderHome)
import View.Choice exposing(renderChoicePage)
import View.Guide exposing(renderGuidePage)
import Message exposing (Page(..))
import View.Collection exposing (renderCollectionPage)
import View.Bgm exposing (..)
{-| This function decides what to view based on the current page
-}
view : Model -> Html Msg
view model =
    let
        ( w , h ) =
            model.windowsize
        level = List.head model.levels
        r = 
            if w / h > 1200 / 800 then
                Basics.min 1 (h / 800)

            else
                Basics.min 1 (w / 1200)
        withoutbgm=

            case model.currentPage of 
                ChoicePage -> renderChoicePage model
                HomePage -> renderHome model
                LevelsPage -> renderLevelPage model
                GuidePage ->
                    if Basics.modBy 2 model.level_index == 0 then
                        renderGuidePage model
                    else renderLevelPage model
                CollectionPage -> renderCollectionPage model
        in
        div[][withoutbgm,gameBGM]