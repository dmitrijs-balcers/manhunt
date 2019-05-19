module Model exposing
    ( LocationsData
    , Model
    , PlayerCoordinate
    , initialModel
    )

import Array
import Craft exposing (AlchemyModel, SelectedCraft, SmithModel, initializeAlchemy, initializeSmithing, noCraftSelected)
import Dict exposing (Dict)
import Map exposing (Direction)
import Player exposing (Player)
import Random


type alias Model =
    { player : Player
    , locationsData : LocationsData
    , worldData : Map.World Map.Height
    , worldSeed : Random.Seed
    , craft : SelectedCraft
    }


initialModel : Model
initialModel =
    { player = Player.initialState
    , locationsData = Dict.empty
    , worldData = Array.empty
    , worldSeed = Random.initialSeed 0
    , craft = noCraftSelected
    }


type alias LocationsData =
    Dict PlayerCoordinate Map.LocationData


type alias PlayerCoordinate =
    ( Int, Int )
