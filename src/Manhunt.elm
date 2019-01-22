module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (element)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h4, h5, img, text)
import Maybe.Extra exposing (filter)
import Maybe exposing (map, withDefault)
import Html.Events exposing (onClick)
import Map exposing (Action(..), Direction(..))
import Player exposing (Player)
import Random exposing (Seed, initialSeed)


type alias LocationsData =
    Dict PlayerCoordinate Map.LocationData


type alias Model =
    { player : Player
    , locationsData : LocationsData
    , worldData : Map.World Map.Height
    , worldSeed : Seed
    }


type Msg
    = Perform Action
    | GenerateWorld Int


type alias PlayerCoordinate =
    ( Int, Int )


initialModel : Model
initialModel =
    { player = Player.initialState
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
                Map.Move direction ->
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
                        |> (\m -> { m | locationsData = refreshLocation m })
                    , Cmd.none
                    )

                Map.Gather gather ->
                    let
                        ( locationsData, player ) =
                            performAction model
                    in
                    ( { model
                        | locationsData = locationsData
                        , player = Player.decreaseStamina player
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
                        Map.generateWorld (Map.Size 150) worldSeed
              }
                |> (\m -> { m | locationsData = refreshLocation m })
            , Cmd.none
            )


performAction : Model -> ( LocationsData, Player )
performAction model =
    let
        position: PlayerCoordinate
        position =
            positionToTuple model.player.position

        currentLocationData : Maybe Map.LocationData
        currentLocationData =
            Dict.get position model.locationsData

        subtract : Maybe Map.LocationData -> Maybe Map.LocationData
        subtract maybeLocationData =
            maybeLocationData
                |> map (\(data, Map.Amount a) -> (data, Map.Amount (a - 1)))
                |> filter (\(_, Map.Amount a) -> a > 0)
    in
    ( Dict.update position subtract model.locationsData
    , Player.updateItems currentLocationData model.player
    )

refreshLocation : Model -> LocationsData
refreshLocation model =
    let
        position : PlayerCoordinate
        position =
            positionToTuple model.player.position
    in
        Map.getLandscape position model.worldData
            |> map (\landscape ->
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


-- VIEW


viewLocationResource : Map.LocationData -> Html msg
viewLocationResource data =
    text (Map.stringifyLocationData data)


view : Model -> Html Msg
view model =
    let
        playerCoordinate : PlayerCoordinate
        playerCoordinate =
            positionToTuple model.player.position

        locationData : Maybe Map.LocationData
        locationData =
            Dict.get playerCoordinate model.locationsData
    in
    div []
        [ Map.viewLocation playerCoordinate model.worldData
        , viewMoveControls
        , Map.viewMap playerCoordinate model.worldData
        , viewResource locationData
        , viewPlayerItems model.player
        ]

positionToTuple : Player.Position -> PlayerCoordinate
positionToTuple ({ lat, lon }) =
    (lat, lon)


viewPlayerItems : Player -> Html Msg
viewPlayerItems player =
    div []
        [ h4 [] [ text "Items:" ]
        , text (Debug.toString player.skills)
        , text (Debug.toString player.stamina)
        , div []
            (List.map
                (\(Map.Resource ( Map.ResourceName resourceName, _ )) ->
                    div [] [ text resourceName ]
                )
                player.items
            )
        ]


viewResource : Maybe Map.LocationData -> Html Msg
viewResource maybeLocationData =
    case maybeLocationData of
        Nothing ->
            text ""

        Just locationData ->
            div []
                [ Map.viewLocationAction Perform locationData
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
    element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
