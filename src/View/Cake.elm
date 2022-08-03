module View.Cake exposing (Caketype(..), renderCake,renderRecipe,renderRecipeStypes)
{-| This library draws different parts of the cake.
-}
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (..)
import Message exposing (SpecialType(..), Stype)
import View.Basic exposing (rectRender)
import Canvas exposing (Renderable)

{-| where the cake appears
-}
type Caketype
    = Progress
    | Recipe
    | Task
    | Collection Int


{-| This function draws toppings on the color sequence.
-}
renderRecipeStypes : List Stype -> List (Html msg)
renderRecipeStypes stypes =
    List.map renderRecipeStype stypes
renderRecipeStype : Stype -> Html msg
renderRecipeStype stype =
    let
        x = 1444 + stype.target*50
        y = 245.5
        item = 
            case stype.content of
                Chocolate -> HtmlAttr.src "./assets/chocolate_grid.png"
                Vanilla -> HtmlAttr.src "./assets/vanilla_grid.png"
    in
        Html.img
            [ item
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat 0.55 ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt (x ) ++ "px")
            , HtmlAttr.style "top" (String.fromFloat ( y) ++ "px")
            ]
            []
{-| This function draws a sequence of color in the recipe section.
-}
renderRecipe : List Color -> List Renderable 
renderRecipe colors = 
    let
        index=List.range 0 (List.length colors)
    in
        List.map2 (renderRect 0 40 40) index colors
renderRect : Float -> Float -> Float -> Int -> Color.Color -> Renderable
renderRect y dx dy x color=
    rectRender (50*toFloat x) y dx dy color
selectDeco : Stype -> Html.Attribute msg
selectDeco stype =
    case stype.content of
        Chocolate ->
            HtmlAttr.src "./assets/chocolate.png"

        Vanilla ->
            HtmlAttr.src "./assets/vanilla.png"

renderProgressDeco : Int -> Stype -> List (Html msg)
renderProgressDeco total stype =
        case stype.state of
            Message.SExit i -> 
                [renderith total 1486 777 5.6 Cream i  (selectDeco stype)]
            _ -> []
renderRecipeDeco: Int -> Stype ->Html msg
renderRecipeDeco total stype =
    renderith total 1486 420 2.8 Cream stype.target  (selectDeco stype)
renderTaskDeco : Int -> Stype ->Html msg
renderTaskDeco total stype =
    renderith total 999 672 5.6 Cream stype.target  (selectDeco stype)
renderDeco : Int -> Int ->Int ->Stype ->Html msg
renderDeco total x y stype =
    renderith total x y 2.8 Cream stype.target  (selectDeco stype)
{-| This function draws the cake with its toppings
-}
renderCake : List Color -> Int -> Int -> Float -> Int -> Caketype -> List Stype -> List (Html msg)
renderCake colors x y scale total caketype stypes=
    let
        index=List.range 0 (List.length colors)
    in
    if total == 0 then
        []

    else
        List.append (List.concat (List.map2 (renderithCake total x y scale stypes caketype) index (List.map selectColor colors))) 
        (renderCandle (List.length colors) total x ((scale / 2) * (0.8 ^ toFloat total)) caketype)


renderCandle : Int -> Int -> Int -> Float -> Caketype -> List (Html msg)
renderCandle now total x scale caketype =
    let
        yo =
            if scale == (1.2 / 2) * (0.8 ^ toFloat total) then
                        if total == 1 then
                            327

                        else if total == 2 then
                            307

                        else if total == 3 then
                            307

                        else if total == 4 then
                            304

                        else if total == 5 then
                            302

                        else if total ==6 then
                            312
                        else
                            302
            else
                        if total == 1 then
                            601

                        else if total == 2 then
                            570

                        else if total == 3 then
                            577

                        else if total == 4 then
                            567

                        else if total == 5 then
                            562
                        else if total ==6 then
                            575
                        else
                            562
        y = 
            case caketype of
                Recipe -> yo
                Progress -> yo
                Task -> (yo-110)
                Collection i-> 
                    if i<=4 then (yo-21)
                    else (yo+156)

    in
    if now == total then
        [ Html.img
            [ HtmlAttr.src "./assets/cake/candles.png"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt (x + 47) ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y) ++ "px")
            ]
            []
        ]

    else
        []
sameLayer : Caketype ->Int -> Stype -> Bool
sameLayer caketype now stype =
    let
        layer =
            case caketype of
                        Progress ->
                            case stype.state of 
                                Message.SExit i ->
                                    i
                                _ -> -1
                        _ -> stype.target
    in
        now==layer
isi: Int -> Int -> Bool
isi i a =
    i==a
renderithCake : Int -> Int -> Int -> Float -> List Stype->Caketype-> Int  -> Html.Attribute msg ->List (Html msg)
renderithCake total x y scale stypes caketype i item =
    let
        layer=List.filter (sameLayer caketype i) stypes
        
        deco =
            case List.head layer of
                Nothing -> []
                Just stype ->
                    case caketype of
                    Progress ->
                        (renderProgressDeco total stype)
                    Recipe -> 
                        [renderRecipeDeco total stype]
                    Task ->
                        [renderTaskDeco total stype]
                    Collection _->
                        [renderDeco total (x+47) (y+33)  stype]
    in 
        List.concat [[(renderith total x y scale Cake i item)],deco]
type Ithtype =Cream|Cake
renderith : Int -> Int -> Int -> Float -> Ithtype-> Int  -> Html.Attribute msg -> Html msg
renderith total x y scale itemtype i item =
    let
        para =
            case total of
                1 ->
                    0.4

                2 ->
                    0.35

                3 ->
                    0.27

                4 ->
                    0.25

                5 ->
                    0.23

                6 ->
                    0.2

                _ ->
                    0.1
    in
    case itemtype of 
        Cake ->
            Html.img
            [ item
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (scale * para * (0.8 ^ toFloat i)) ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y - (80.0 * scale * para * ((1 - 0.8 ^ toFloat i) / 0.2 + 1))) ++ "px")
            ]
            []
        Cream ->
            Html.img
            [ item
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat (scale * para * (0.8 ^ toFloat i)) ++ ")")
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" (String.fromInt x ++ "px")
            , HtmlAttr.style "top" (String.fromFloat (toFloat y - (80.0 * scale/2.8*1.2 * para * ((1 - 0.8 ^ toFloat i) / 0.2 + 1))) ++ "px")
            ]
            []


selectColor : Color -> Html.Attribute msg
selectColor color =
    if color == red then
        HtmlAttr.src "./assets/cake/red.png"

    else if color == green then
        HtmlAttr.src "./assets/cake/green.png"

    else if color == orange then
        HtmlAttr.src "./assets/cake/orange.png"

    else if color == lightYellow then
        HtmlAttr.src "./assets/cake/yellow.png"

    else if color == blue then
        HtmlAttr.src "./assets/cake/blue.png"

    else if color == white then
        HtmlAttr.src "./assets/cake/white.png"

    else if color == purple then
        HtmlAttr.src "./assets/cake/purple.png"

    else
        HtmlAttr.src "./assets/cake/grey.png"
