module Manhunt exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Events
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (Attribute, Html, button, div, h4, h5, img, text)
import Html.Events exposing (keyCode, onClick, preventDefaultOn)
import Json.Decode as Decode
import Map exposing (Action(..), Direction(..))
import Maybe exposing (map, withDefault)
import Model exposing (KeyboardAction, LocationsData, Model, OtherKey(..), PlayerCoordinate)
import Msg exposing (Msg(..))
import Platform.Sub exposing (Sub)
import Player exposing (Player)
import Random exposing (Seed, initialSeed)
import Time
import Update exposing (positionToTuple, refreshLocation)


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
        Keyboard keyAction ->
            case enoughStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
                    case keyAction of
                        Either.Left direction ->
                            Update.move model direction

                        Either.Right other ->
                            case other of
                                SpaceBar ->
                                    Update.gather model

                                Other _ ->
                                    ( model, Cmd.none )

        Perform action ->
            case enoughStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
                    case action of
                        Map.Move direction ->
                            Update.move model direction

                        Map.Gather _ ->
                            Update.gather model

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
    div [ onKeyDown ]
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


toDirection : Int -> KeyboardAction
toDirection key =
    case Debug.log "pressed key#" key of
        37 ->
            Either.Left West

        38 ->
            Either.Left North

        39 ->
            Either.Left East

        40 ->
            Either.Left South

        32 ->
            Either.Right SpaceBar

        _ ->
            Either.Right (Other key)


onKeyDown : Attribute Msg
onKeyDown =
    Decode.map toDirection keyCode
        |> Decode.map Keyboard
        |> Decode.map alwaysPreventDefault
        |> preventDefaultOn "keydown"


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick ]


main : Program () Model Msg
main =
    element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
