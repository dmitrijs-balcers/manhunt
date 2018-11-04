module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h4, h5, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Map
    exposing
        ( Action(..)
        , Amount
        , Direction(..)
        , Gather
        , Height
        , LocationData
        , World
        , generateLocationData
        , generateWorld
        , getLandscape
        , landscapeToString
        , stringifyLocationData
        , viewLocationAction
        , viewMap
        )
import Player exposing (Player, PlayerPosition, Stamina, updateItems, updatePlayerPosition)
import Random exposing (Seed, initialSeed)
import Result
import Simplex exposing (simplex2D)


type alias LocationsData =
    Dict PlayerCoordinate LocationData


type alias Model =
    { player : Player
    , locationsData : LocationsData
    , worldData : World Height
    , worldSeed : Seed
    }


type Msg
    = Perform Action
    | GenerateWorld Int


type alias PlayerCoordinate =
    ( Int, Int )


initialModel : Model
initialModel =
    { player =
        { position = { lat = 0, lon = 0 }
        , items = []
        , skills = { strength = 0 }
        , stamina = Player.Stamina 100
        }
    , locationsData = Dict.empty
    , worldData = Array.empty
    , worldSeed = initialSeed 0
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Perform action ->
            case action of
                Move direction ->
                    let
                        ( _, seed ) =
                            Random.step (Random.int 1 6000) model.worldSeed
                    in
                    ( { model
                        | worldSeed = seed
                        , player = updatePlayerPosition model.player direction
                      }
                        |> (\m -> { m | locationsData = refreshLocation m })
                    , Cmd.none
                    )

                Gather gather ->
                    let
                        ( locationsData, player ) =
                            performAction model
                    in
                    ( { model
                        | locationsData = locationsData
                        , player = player
                      }
                    , Cmd.none
                    )

        GenerateWorld randomNumber ->
            let
                worldSeed =
                    initialSeed randomNumber
            in
            ( { model
                | worldSeed = worldSeed
                , worldData =
                    if Array.length model.worldData > 0 then
                        model.worldData

                    else
                        generateWorld (Map.Size 150) worldSeed
              }
                |> (\m -> { m | locationsData = refreshLocation m })
            , Cmd.none
            )


performAction : Model -> ( LocationsData, Player )
performAction model =
    let
        position : PlayerCoordinate
        position =
            ( model.player.position.lat, model.player.position.lon )

        currentLocationData : Maybe LocationData
        currentLocationData =
            Dict.get position model.locationsData

        subtract : Maybe LocationData -> Maybe LocationData
        subtract maybeLocationData =
            case maybeLocationData of
                Just ( ( resource, action ), Map.Amount amount ) ->
                    let
                        newAmount : Int
                        newAmount =
                            amount - 1
                    in
                    if newAmount == 0 then
                        Nothing

                    else
                        Just ( ( resource, action ), Map.Amount newAmount )

                Nothing ->
                    maybeLocationData
    in
    ( Dict.update position subtract model.locationsData
    , updateItems currentLocationData model.player
    )


refreshLocation : Model -> LocationsData
refreshLocation model =
    let
        position : PlayerCoordinate
        position =
            ( model.player.position.lat, model.player.position.lon )

        locationData : Maybe LocationData
        locationData =
            Dict.get position model.locationsData
    in
    case locationData of
        Just data ->
            {-
               There should be delay between the updates
               If location data were not generated at first we should regenerate after X time
            -}
            Debug.log "location data already exists" model.locationsData

        Nothing ->
            case getLandscape position model.worldData of
                Just landscape ->
                    let
                        generatedLocationData : Maybe LocationData
                        generatedLocationData =
                            Tuple.first (Random.step (generateLocationData landscape) model.worldSeed)
                    in
                    case generatedLocationData of
                        Just ld ->
                            Dict.insert position ld model.locationsData

                        Nothing ->
                            model.locationsData

                Nothing ->
                    model.locationsData



-- VIEW


viewLocationResource : LocationData -> Html msg
viewLocationResource data =
    text (stringifyLocationData data)


view : Model -> Html Msg
view model =
    let
        playerCoordinate : PlayerCoordinate
        playerCoordinate =
            ( model.player.position.lat, model.player.position.lon )

        locationData : Maybe LocationData
        locationData =
            Dict.get playerCoordinate model.locationsData
    in
    div []
        [ Map.viewLocation playerCoordinate model.worldData
        , viewMoveControls
        , viewMap playerCoordinate model.worldData
        , viewResource locationData
        , viewPlayerItems model.player
        ]


viewPlayerItems : Player -> Html Msg
viewPlayerItems player =
    div []
        [ h4 [] [ text "Items:" ]
        , text (Debug.toString player.skills)
        , text (Debug.toString player.stamina)
        , div []
            (List.map
                (\(Map.Resource resource) ->
                    let
                        ( Map.ResourceName resourceName, _ ) =
                            resource
                    in
                    div [] [ text resourceName ]
                )
                player.items
            )
        ]


viewResource : Maybe LocationData -> Html Msg
viewResource maybeLocationData =
    case maybeLocationData of
        Nothing ->
            text ""

        Just locationData ->
            div []
                [ viewLocationAction Perform locationData
                , viewLocationResource locationData
                ]


viewMoveControls : Html Msg
viewMoveControls =
    div []
        [ button [ onClick (Perform (Move North)) ] [ text "N" ]
        , button [ onClick (Perform (Move West)) ] [ text "W" ]
        , button [ onClick (Perform (Move East)) ] [ text "E" ]
        , button [ onClick (Perform (Move South)) ] [ text "S" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
