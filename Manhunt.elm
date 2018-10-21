module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Map exposing (Action, LocationData, generateLocationData, getLandscape, landscapeToString, stringifyLocationData, viewLocation, viewLocationAction, viewMap)
import Random
import Result


type alias PlayerPosition =
    { lat : Int, lon : Int }


type alias LocationsData =
    Dict ( Int, Int ) (Maybe LocationData)


type alias Model =
    { playerPosition : PlayerPosition
    , locationsData : LocationsData
    }


type Direction
    = North
    | South
    | West
    | East


type Msg
    = Move Direction
    | Perform Action
    | UpdateLocationData Int


type alias PlayerCoordinate =
    ( Int, Int )


initialModel =
    { playerPosition = { lat = 0, lon = 0 }
    , locationsData = Dict.empty
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            ( { model | playerPosition = updatePlayerPosition model.playerPosition direction }
            , Random.generate UpdateLocationData (Random.int 1 6000)
            )

        UpdateLocationData randomNumber ->
            ( { model | locationsData = updateLocationData randomNumber model }
            , Cmd.none
            )

        Perform action ->
            case action of
                _ ->
                    ( model, Cmd.none )


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


updateLocationData : Int -> Model -> LocationsData
updateLocationData randomNumber model =
    let
        position : PlayerCoordinate
        position =
            ( model.playerPosition.lat, model.playerPosition.lon )

        locationData : Maybe (Maybe LocationData)
        locationData =
            Dict.get position model.locationsData
    in
    case locationData of
        Just data ->
            Debug.log "location data already exists" model.locationsData

        Nothing ->
            case getLandscape position of
                Just landscape ->
                    Dict.insert position (generateLocationData randomNumber landscape) model.locationsData

                Nothing ->
                    model.locationsData



-- VIEW


viewLocationResource : Maybe LocationData -> Html msg
viewLocationResource data =
    case data of
        Just s ->
            text (stringifyLocationData s)

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    if Dict.size model.locationsData == 0 then
        text ""

    else
        let
            playerCoordinate : PlayerCoordinate
            playerCoordinate =
                ( model.playerPosition.lat, model.playerPosition.lon )

            locationsData : LocationsData
            locationsData =
                model.locationsData

            locationData : Maybe LocationData
            locationData =
                findSafeInDict playerCoordinate locationsData
        in
        div []
            [ viewLocation playerCoordinate
            , viewMoveControls
            , viewLocationAction Perform locationData
            , viewLocationResource locationData
            , viewMap playerCoordinate
            ]


findSafeInDict : ( Int, Int ) -> LocationsData -> Maybe LocationData
findSafeInDict id dict =
    case Dict.get id dict of
        Just res ->
            res

        Nothing ->
            Debug.todo ("Didn't find " ++ Debug.toString id ++ " in " ++ Debug.toString dict)


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
        { init = \flags -> ( initialModel, Random.generate UpdateLocationData (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
