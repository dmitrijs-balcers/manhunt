module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Map exposing (Action, Amount, Height, LocationData, World, generateLocationData, generateWorld, getLandscape, landscapeToString, stringifyLocationData, viewLocation, viewLocationAction, viewMap)
import Random exposing (initialSeed)
import Result
import Simplex exposing (simplex2D)


type alias PlayerPosition =
    { lat : Int, lon : Int }


type alias LocationsData =
    Dict PlayerCoordinate LocationData


type alias Model =
    { playerPosition : PlayerPosition
    , locationsData : LocationsData
    , worldData : World Height
    }


type Direction
    = North
    | South
    | West
    | East


type Msg
    = Move Direction
    | Perform Action
    | GenerateWorld Int


type alias PlayerCoordinate =
    ( Int, Int )


initialModel : Model
initialModel =
    { playerPosition = { lat = 0, lon = 0 }
    , locationsData = Dict.empty
    , worldData = Array.empty
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            ( { model | playerPosition = updatePlayerPosition model.playerPosition direction }
            , Random.generate GenerateWorld (Random.int 1 6000)
            )

        GenerateWorld randomNumber ->
            ( { model
                | locationsData = refreshLocation randomNumber model
                , worldData =
                    if Array.length model.worldData > 0 then
                        model.worldData

                    else
                        generateWorld (Map.Size 150) (initialSeed randomNumber)
              }
            , Cmd.none
            )

        Perform action ->
            ( { model | locationsData = performAction model }, Cmd.none )


performAction : Model -> LocationsData
performAction model =
    let
        position : PlayerCoordinate
        position =
            ( model.playerPosition.lat, model.playerPosition.lon )

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
    Dict.update position subtract model.locationsData


updatePlayerPosition : PlayerPosition -> Direction -> PlayerPosition
updatePlayerPosition playerPosition direction =
    case direction of
        North ->
            { playerPosition | lon = playerPosition.lon + 1 }

        South ->
            { playerPosition | lon = playerPosition.lon - 1 }

        West ->
            { playerPosition | lat = playerPosition.lat + 1 }

        East ->
            { playerPosition | lat = playerPosition.lat - 1 }


refreshLocation : Int -> Model -> LocationsData
refreshLocation seed model =
    let
        position : PlayerCoordinate
        position =
            ( model.playerPosition.lat, model.playerPosition.lon )

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
                            generateLocationData seed landscape
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
            ( model.playerPosition.lat, model.playerPosition.lon )

        locationsData : LocationsData
        locationsData =
            model.locationsData

        locationData : Maybe LocationData
        locationData =
            Dict.get playerCoordinate locationsData
    in
    div []
        [ viewLocation playerCoordinate model.worldData
        , viewMoveControls
        , viewResource locationData
        , viewMap playerCoordinate model.worldData
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
        [ button [ onClick (Move North) ] [ text "N" ]
        , button [ onClick (Move West) ] [ text "W" ]
        , button [ onClick (Move East) ] [ text "E" ]
        , button [ onClick (Move South) ] [ text "S" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
