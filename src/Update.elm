module Update exposing (move, positionToTuple, refreshLocation)

import Dict
import Map exposing (Direction(..))
import Maybe exposing (map, withDefault)
import Model exposing (LocationsData, Model, PlayerCoordinate)
import Msg exposing (Msg)
import Player
import Random


move : Model -> Direction -> ( Model, Cmd Msg )
move model direction =
    let
        ( _, seed ) =
            Random.step (Random.int 1 6000) model.worldSeed
    in
    ( { model
        | worldSeed = seed
        , player =
            model.player
                |> Player.updatePosition direction
                |> Player.decreaseStamina
      }
        |> (\m ->
                positionToTuple m.player.position
                    |> (\position -> Dict.get position m.locationsData)
                    |> (\locationData ->
                            case locationData of
                                Nothing ->
                                    { m | locationsData = refreshLocation m }

                                Just _ ->
                                    m
                       )
           )
    , Cmd.none
    )


positionToTuple : Player.Position -> PlayerCoordinate
positionToTuple { lat, lon } =
    ( lat, lon )


refreshLocation : Model -> LocationsData
refreshLocation model =
    let
        position : PlayerCoordinate
        position =
            positionToTuple model.player.position
    in
    position
        |> Map.getLandscape model.worldData
        |> map
            (\landscape ->
                let
                    generatedLocationData : Maybe Map.LocationData
                    generatedLocationData =
                        Tuple.first (Random.step (Map.generateLocationData landscape) model.worldSeed)
                in
                generatedLocationData
                    |> map (\ld -> Dict.insert position ld model.locationsData)
                    |> withDefault model.locationsData
            )
        |> withDefault model.locationsData
