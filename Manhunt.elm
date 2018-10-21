module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Map
    exposing
        ( Action
        , LocationData
        , generateLocationData
        , getLandscape
        , landscapeToString
        , stringifyLocationData
        , viewLocation
        , viewLocationAction
        , viewMap
        )
import Random
import Result


type alias PlayerPosition =
    { lat : Int, lon : Int }


type alias LocationsData =
    Dict ( Int, Int ) LocationData


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
        position =
            ( model.playerPosition.lat, model.playerPosition.lon )
    in
    case Dict.get position model.locationsData of
        Just data ->
            model.locationsData

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
    let
        playerCoordinate =
            ( model.playerPosition.lat, model.playerPosition.lon )

        locationsData =
            model.locationsData
    in
    div []
        [ viewLocation playerCoordinate
        , viewMoveControls
        , Dict.get playerCoordinate locationsData |> viewLocationAction Perform
        , Dict.get playerCoordinate locationsData |> viewLocationResource
        , viewMap playerCoordinate
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
        { init = \flags -> ( initialModel, Random.generate UpdateLocationData (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
