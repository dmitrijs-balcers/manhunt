module Model exposing (KeyboardAction, LocationsData, Model, Other, PlayerCoordinate)

import Dict exposing (Dict)
import Either exposing (Either)
import Map exposing (Direction)
import Player exposing (Player)
import Random exposing (Seed)


type alias Model =
    { player : Player
    , locationsData : LocationsData
    , worldData : Map.World Map.Height
    , worldSeed : Seed
    }


type alias LocationsData =
    Dict PlayerCoordinate Map.LocationData


type alias KeyboardAction =
    Either Direction Other


type alias Other =
    String


type alias PlayerCoordinate =
    ( Int, Int )
