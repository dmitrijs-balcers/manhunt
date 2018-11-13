module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h4, h5, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Map exposing (Action(..), Direction(..))
import Player exposing (Player)
import Random exposing (Seed, initialSeed)
import Result
import Simplex exposing (simplex2D)


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
                        , player =
                            player
                                |> Player.decreaseStamina
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
        position : PlayerCoordinate
        position =
            ( model.player.position.lat, model.player.position.lon )

        currentLocationData : Maybe Map.LocationData
        currentLocationData =
            Dict.get position model.locationsData

        subtract : Maybe Map.LocationData -> Maybe Map.LocationData
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
    , Player.updateItems currentLocationData model.player
    )


refreshLocation : Model -> LocationsData
refreshLocation model =
    let
        position : PlayerCoordinate
        position =
            ( model.player.position.lat, model.player.position.lon )

        locationData : Maybe Map.LocationData
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
            case Map.getLandscape position model.worldData of
                Just landscape ->
                    let
                        generatedLocationData : Maybe Map.LocationData
                        generatedLocationData =
                            Tuple.first (Random.step (Map.generateLocationData landscape) model.worldSeed)
                    in
                    case generatedLocationData of
                        Just ld ->
                            Dict.insert position ld model.locationsData

                        Nothing ->
                            model.locationsData

                Nothing ->
                    model.locationsData



-- VIEW


viewLocationResource : Map.LocationData -> Html msg
viewLocationResource data =
    text (Map.stringifyLocationData data)


view : Model -> Html Msg
view model =
    let
        playerCoordinate : PlayerCoordinate
        playerCoordinate =
            ( model.player.position.lat, model.player.position.lon )

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
    Browser.element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
