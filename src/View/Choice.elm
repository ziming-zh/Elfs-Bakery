module View.Choice exposing (renderChoicePage)
{-| This library renders the Choice Page.
-}
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton,renderButtonColor)
import View.Basic exposing (renderChoiceButton)
import View.Hat exposing (hat)
import View.Cake exposing (renderCake)
import View.Cake exposing (Caketype(..))
import View.Basic exposing (renderTxt)

renderChoicePage : Model -> Html Msg
renderChoicePage model =
    let
        ( w , h ) =
            model.windowsize
        r = 
            if w / h > 1200 / 800 then
                (h / 800)

            else
                Basics.min 1 (w / 1200)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat 1200 ++ "px")
        , HtmlAttr.style "height"  (String.fromFloat 800 ++ "px")
        , HtmlAttr.style "left" (String.fromFloat ((w - 1200 * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - 800 * r) / 2) ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        ]
        (List.concat [ [ Html.img
            [ HtmlAttr.src "./assets/gamepage/level.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromFloat 0 ++ "px")
            , HtmlAttr.style "top" (String.fromFloat 87 ++ "px")
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 1.328 ++ ")")
            ][]
        , renderChoiceButton "#F4B183" "1" (LoadLevel 1) (ChoiceInfo 1) (23,395) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "2" (LoadLevel 2) (ChoiceInfo 2) (186,395) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "3" (LoadLevel 3) (ChoiceInfo 3) (349,395) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "4" (LoadLevel 4) (ChoiceInfo 4) (512,395) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "5" (LoadLevel 5) (ChoiceInfo 5) (23,527) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "6" (LoadLevel 6) (ChoiceInfo 6) (186,527) 1 (65,65) "#FFFFFF"
        , renderChoiceButton "#F4B183" "7" (LoadLevel 7) (ChoiceInfo 7) (349,527) 1 (65,65) "#FFFFFF"
        , renderButtonColor "#4472C4" "<" (Load HomePage) (-60,0) 1 (50,50) "#FFFFFF"
        
        
        --, renderButtonColor "#F4B183" "2" (LoadLevel 2) (100,395) 1 (260,66) "#FFFFFF"
        --, renderButton "Level 3" (LoadLevel 1) (880,320) 1 (260,66) "#FFFFFF"
        ], (renderGameInfo model)
        ])


description1 : String
description1 ="Now that you have mastered the skills of merging and creating colors, take advantage of them and help Little Elf make colorful cakes!"
description2 : String
description2 ="Cream is a precious resource! We have to make full use of them and avoid wastes! "
description4 : String
description4 ="Although the map could be the same, the different positions of toppings and cake types make endless of possibilities."
description3 : String
description3 ="The toppings of vanilla and chocolate sprinkling on the cake makes it more appealing! But remember: it always stays still until the cream flows along!"
description5 : String
description5 ="After finishing past levels, you have mastered the rules of flowing: The creams always rush towards the exit! Make use of this characteristic to pass this level!"
description6 : String
description6 ="This is an comprehensive level which invites you to practice combining all the skills you have learned!"
description7 : String
description7 ="Now you have mastered all the skills! The Little Elf is also facing the final challenge to become the Cake Master! Help him get the Gusteau Chef Award!"
descriptions : List String
descriptions =[description1,description2,description3,description4,description5,description6,description7]
renderHat: Int -> Html Msg
renderHat i =
    Html.img
            [ hat i
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 0.7 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" ("1028px")
            , HtmlAttr.style "top" ("88px")
            ]
            []

renderGameInfo : Model -> List (Html Msg)
renderGameInfo model =
    let i=model.level_index
    in
    if i <= 7 && i >=1 then
        let 
            levels =
                List.drop (i-1)  model.levels
            (colors,stypes) = 
                case List.head levels of
                    Just lv-> (lv.colorseq,lv.stypes)
                    Nothing -> ([],[])
            description =
                case List.head (List.drop (i-1)  descriptions) of 
                    Just des
                        -> des
                    Nothing -> ""
            sizei = 18
        in
            List.concat 
            [[renderHat i]
            , (renderCake colors 950 640 2.4 (List.length colors) View.Cake.Task stypes)
            , [renderTxt (900,230) sizei  "#000000" description  (1)]]
    else 
        []