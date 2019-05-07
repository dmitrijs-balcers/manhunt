module Model exposing (LocationsData, Model, PlayerCoordinate)

import Dict exposing (Dict)
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


type alias PlayerCoordinate =
    ( Int, Int )
