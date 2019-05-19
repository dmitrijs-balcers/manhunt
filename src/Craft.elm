module Craft exposing
    ( AlchemyModel
    , Potion
    , SelectedCraft
    , SmithModel
    , initializeAlchemy
    , initializeSmithing
    , noCraftSelected
    , selectAlchemy
    , selectCraft
    , selectSmith
    , viewCraft
    )

import Html exposing (Html, div, text)
import Msg exposing (Craft(..))
import Resource exposing (Effect, Flower, Intensity, Mushroom, Resource, Rock, flowerEffect)


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
    AlchemyCraft initializeAlchemy


selectSmith : SelectedCraft
selectSmith =
    SmithingCraft initializeSmithing


type Potion
    = Potion (List ( Effect, Intensity ))


type AlchemyResource
    = FlowerResource Flower
    | MushroomResource Mushroom


type alias AlchemyModel =
    { items : List AlchemyResource
    }


initializeAlchemy : AlchemyModel
initializeAlchemy =
    { items = []
    }


type SmithResource
    = SmithResource Rock


type alias SmithModel =
    { items : List SmithResource
    }


initializeSmithing : SmithModel
initializeSmithing =
    { items = []
    }


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
