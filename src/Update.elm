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
    ( (updateWorldSeed >> movePlayer direction >> refreshLocationIfResourceMissing) model, Cmd.none )


updateWorldSeed : Model -> Model
updateWorldSeed model =
    let
        ( _, seed ) =
            Random.step (Random.int 1 6000) model.worldSeed
    in
    { model | worldSeed = seed }


movePlayer : Direction -> Model -> Model
movePlayer direction model =
    { model
        | player =
            model.player
                |> Player.updatePosition direction
                |> Player.decreaseStamina
    }


refreshLocationIfResourceMissing : Model -> Model
refreshLocationIfResourceMissing model =
    Maybe.withDefault
        { model | locationsData = refreshLocation model }
        (Maybe.map (\_ -> model) (getLocationData model))


getLocationData : Model -> Maybe Map.LocationData
getLocationData model =
    positionToTuple model.player.position
        |> (\position -> Dict.get position model.locationsData)


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
                |> map (\( resource, g ) -> ( Resource.takeOne resource, g ))
                |> filter (\( resource, _ ) -> Resource.getAmount resource > 0)
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
