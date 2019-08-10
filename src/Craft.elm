module Craft exposing
    ( AlchemyModel
    , Potion
    , SelectedCraft(..)
    , SmithModel
    , addAlchemyResource
    , addToAlchemyCraft
    , addToSmithingCraft
    , noCraftSelected
    , selectAlchemy
    , selectCraft
    , selectSmith
    , viewCraft
    , viewCraftButton
    )

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Msg exposing (Craft(..), Msg)
import Resource
    exposing
        ( AlchemyResource
        , Effect
        , Flower
        , Intensity
        , Mushroom
        , Resource
        , Rock
        , SmithResource
        , alchemyResource
        , flowerEffect
        , isSmithingResource
        )


type SelectedCraft
    = AlchemyCraft AlchemyModel
    | SmithingCraft SmithModel
    | NoCraftSelected


selectCraft : SelectedCraft -> Craft -> SelectedCraft
selectCraft currentSelectedCraft craft =
    case craft of
        Alchemy ->
            if currentSelectedCraft == selectAlchemy then
                NoCraftSelected

            else
                selectAlchemy

        Smith ->
            if currentSelectedCraft == selectSmith then
                NoCraftSelected

            else
                selectSmith


noCraftSelected : SelectedCraft
noCraftSelected =
    NoCraftSelected


selectAlchemy : SelectedCraft
selectAlchemy =
    AlchemyCraft initializeCraftingModel


selectSmith : SelectedCraft
selectSmith =
    SmithingCraft initializeCraftingModel


type Potion
    = Potion (List ( Effect, Intensity ))


type alias CrafringModel x =
    { items : List x }


type alias AlchemyModel =
    CrafringModel AlchemyResource


initializeCraftingModel : CrafringModel x
initializeCraftingModel =
    { items = []
    }


addToAlchemyCraft : SelectedCraft -> AlchemyResource -> SelectedCraft
addToAlchemyCraft craft resource =
    case craft of
        AlchemyCraft alchemyModel ->
            AlchemyCraft (addAlchemyResource alchemyModel resource)

        _ ->
            craft


addAlchemyResource : CrafringModel AlchemyResource -> AlchemyResource -> CrafringModel AlchemyResource
addAlchemyResource model res =
    { model | items = res :: model.items }


addToSmithingCraft : SelectedCraft -> SmithResource -> SelectedCraft
addToSmithingCraft craft resource =
    case craft of
        SmithingCraft smithModel ->
            SmithingCraft (addSmithingResource smithModel resource)

        _ ->
            craft


addSmithingResource : CrafringModel SmithResource -> SmithResource -> CrafringModel SmithResource
addSmithingResource model res =
    { model | items = res :: model.items }


type alias SmithModel =
    CrafringModel SmithResource


brew : Flower -> Potion -> Potion
brew flower (Potion effects) =
    Potion (flowerEffect flower :: effects)



-- View


viewCraft : SelectedCraft -> Html msg
viewCraft craft =
    case craft of
        AlchemyCraft model ->
            div [] [ text "Alchemy selected" ]

        SmithingCraft model ->
            div [] [ text "Smithing selected" ]

        NoCraftSelected ->
            div [] [ text "Nothing selected" ]


viewCraftButton : SelectedCraft -> Resource -> Html Msg
viewCraftButton craft resource =
    case craft of
        AlchemyCraft _ ->
            alchemyResource resource
                |> Maybe.map (\r -> button [ onClick (Msg.AddToAlchemyCraft r) ] [ text "+" ])
                |> Maybe.withDefault (text "")

        SmithingCraft _ ->
            isSmithingResource resource
                |> Maybe.map (\r -> button [ onClick (Msg.AddToSmithingCraft r) ] [ text "+" ])
                |> Maybe.withDefault (text "")

        NoCraftSelected ->
            text ""
