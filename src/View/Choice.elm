module View.Choice exposing (..)
import Model exposing (Model)
import Message exposing (Msg(..),Page(..))
import Html.Attributes as HtmlAttr exposing (..)
import Html exposing (Html, div)
import View.Basic exposing (renderButton,renderButtonColor)
import View.Basic exposing (renderChoiceButton)
import View.Hat exposing (hat)
import View.Cake exposing (renderCake,renderTaskDeco)
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
        , renderChoiceButton "#4472C4" "<" (Load HomePage) (-60,0) 1 (50,50) "#FFFFFF"
        
        
        --, renderButtonColor "#F4B183" "2" (LoadLevel 2) (100,395) 1 (260,66) "#FFFFFF"
        --, renderButton "Level 3" (LoadLevel 1) (880,320) 1 (260,66) "#FFFFFF"
        ], (renderGameInfo model)
        ])


description1 : String
description1 ="Little Elf is a beginner. After reading the recipes,\n he decides to try an easy cake at first. \nElf's favourite fruit is mango, straberry and cherry. \nPlease help him to make this cake!"
description3 : String
description3 ="After years of success, Little Elf's store enjoys a good reputation. Here comes the good news! The Gusteau Chef warmly invites to Little Elf to participate in a competition. The Elf won't miss this golden chance. He decides to make the masterpiece of his father -- the supreme rosy champagne milk cake with vanilla topping."
description2 : String
description2 ="After months of practice, Little Elf masters some techniques to make cakes, so he reopens the bakery. The first customer is a little girl who loves chocolates and she orders a mango cake with chocolate topping."
descriptions : List String
descriptions =[description1,description2,description3]
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
            sizei = 
                if i ==3 then 15
                    else 18
        in
            List.concat 
            [[renderHat i]
            , (renderCake colors 950 640 2.4 (List.length colors) View.Cake.Task)
            , [renderTxt (900,230) sizei  "#000000" description  (1)]
            , List.map (renderTaskDeco (List.length colors)) stypes]
    else 
        []