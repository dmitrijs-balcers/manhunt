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
    | RefreshLocation Int


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
            , Random.generate RefreshLocation (Random.int 1 6000)
            )

        RefreshLocation randomNumber ->
            ( { model | locationsData = refreshLocation randomNumber model }
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

        locationData : LocationData
        locationData =
            case findSafeInDict position model.locationsData of
                Just data ->
                    data

                Nothing ->
                    Debug.todo "Shouldn't be possible perform action if no data"

        subtract : Maybe (Maybe LocationData) -> Maybe (Maybe LocationData)
        subtract maybeMaybeLocationData =
            case maybeMaybeLocationData of
                Just maybeLocationData ->
                    case maybeLocationData of
                        Just ( ( resource, action ), amount ) ->
                            let
                                newAmount : Int
                                newAmount =
                                    amount - 1
                            in
                            if newAmount == 0 then
                                Just Nothing

                            else
                                Just (Just ( ( resource, action ), newAmount ))

                        Nothing ->
                            maybeMaybeLocationData

                Nothing ->
                    maybeMaybeLocationData
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

        locationData : Maybe (Maybe LocationData)
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
            case getLandscape position of
                Just landscape ->
                    Dict.insert position (generateLocationData seed landscape) model.locationsData

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
        { init = \flags -> ( initialModel, Random.generate RefreshLocation (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
