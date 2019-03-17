module Update exposing (gather, move, positionToTuple, refreshLocation)

import Dict
import Map exposing (Direction(..))
import Maybe exposing (map, withDefault)
import Maybe.Extra exposing (filter)
import Model exposing (LocationsData, Model, PlayerCoordinate)
import Msg exposing (Msg)
import Player exposing (Player)
import Random
import Resource


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


gather : Model -> ( Model, Cmd Msg )
gather model =
    performGather model
        |> (\( locationsData, player ) ->
                ( { model
                    | locationsData = locationsData
                    , player = Player.decreaseStamina player
                  }
                , Cmd.none
                )
           )


performGather : Model -> ( LocationsData, Player )
performGather model =
    let
        subtract : Maybe Map.LocationData -> Maybe Map.LocationData
        subtract maybeLocationData =
            maybeLocationData
                |> map (\( data, Resource.Amount a ) -> ( data, Resource.Amount (a - 1) ))
                |> filter (\( _, Resource.Amount a ) -> a > 0)
    in
    positionToTuple model.player.position
        |> (\position ->
                ( Dict.update position subtract model.locationsData
                , Dict.get position model.locationsData
                    |> map (Player.updateItems model.player)
                    |> withDefault model.player
                )
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
