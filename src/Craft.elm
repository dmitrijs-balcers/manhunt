module Craft exposing
    ( AlchemyModel
    , Potion
    , SelectedCraft
    , SmithModel
    , noCraftSelected
    , selectAlchemy
    , selectCraft
    , selectSmith
    , viewCraft
    , viewCraftButton
    )

import Html exposing (Html, button, div, text)
import Msg exposing (Craft(..))
import Resource exposing (Effect, Flower, Intensity, Mushroom, Resource, Rock, flowerEffect, isAlchemyResource, isSmithingResource)


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


type AlchemyResource
    = FlowerResource Flower
    | MushroomResource Mushroom


type alias AlchemyModel =
    CrafringModel AlchemyResource


initializeCraftingModel : CrafringModel x
initializeCraftingModel =
    { items = []
    }


type SmithResource
    = SmithResource Rock


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


viewCraftButton : SelectedCraft -> Resource -> Html msg
viewCraftButton craft resource =
    case craft of
        AlchemyCraft _ ->
            isAlchemyResource resource
                |> Maybe.map (\r -> button [] [ text "+" ])
                |> Maybe.withDefault (text "")

        SmithingCraft _ ->
            isSmithingResource resource
                |> Maybe.map (\r -> button [] [ text "+" ])
                |> Maybe.withDefault (text "")

        NoCraftSelected ->
            text ""
