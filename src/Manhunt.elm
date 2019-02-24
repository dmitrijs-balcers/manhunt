module Manhunt exposing (Model, main)

import Array exposing (Array)
import Browser exposing (element)
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (Html, button, div, h4, h5, img, text)
import Html.Events exposing (onClick)
import Map exposing (Action(..), Direction(..))
import Maybe exposing (map, withDefault)
import Maybe.Extra exposing (filter)
import Player exposing (Player)
import Random exposing (Seed, initialSeed)
import Time


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
    | Tick Time.Posix


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
            case enoughStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
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

                        Map.Gather _ ->
                            performAction model
                                |> (\( locationsData, player ) ->
                                        ( { model
                                            | locationsData = locationsData
                                            , player = Player.decreaseStamina player
                                          }
                                        , Cmd.none
                                        )
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

        Tick posix ->
            case tooMuchStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
                    ( { model | player = Player.increaseStamina model.player }, Cmd.none )


enoughStamina : Model -> Either Model Model
enoughStamina model =
    model.player.stamina
        |> (\(Player.Stamina stamina) ->
                if stamina <= 0 then
                    Either.Left model

                else
                    Either.Right model
           )


tooMuchStamina : Model -> Either Model Model
tooMuchStamina model =
    model.player.stamina
        |> (\(Player.Stamina stamina) ->
                if stamina >= 100 then
                    Either.Left model

                else
                    Either.Right model
           )


performAction : Model -> ( LocationsData, Player )
performAction model =
    let
        subtract : Maybe Map.LocationData -> Maybe Map.LocationData
        subtract maybeLocationData =
            maybeLocationData
                |> map (\( data, Map.Amount a ) -> ( data, Map.Amount (a - 1) ))
                |> filter (\( _, Map.Amount a ) -> a > 0)
    in
    positionToTuple model.player.position
        |> (\position ->
                ( Dict.update position subtract model.locationsData
                , Dict.get position model.locationsData
                    |> map (Player.updateItems model.player)
                    |> withDefault model.player
                )
           )


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
positionToTuple { lat, lon } =
    ( lat, lon )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


main : Program () Model Msg
main =
    element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
