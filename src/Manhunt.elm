module Manhunt exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Craft exposing (SelectedCraft, selectAlchemy, selectCraft, selectSmith, viewCraft)
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (Attribute, Html, a, button, div, h4, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Map exposing (Action(..), Direction(..))
import Model exposing (LocationsData, Model, PlayerCoordinate, initialModel)
import Msg exposing (Craft(..), Msg(..))
import Platform.Sub exposing (Sub)
import Player exposing (Player)
import Port
import Random exposing (Seed, initialSeed)
import Resource exposing (Resource)
import Time
import Update exposing (positionToTuple, refreshLocation)


type KeyboardAction
    = Direction Direction
    | OtherKey OtherKey


type OtherKey
    = SpaceBar
    | Other String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keyboard keyAction ->
            case enoughStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
                    case toDirection keyAction of
                        Direction direction ->
                            Update.move model direction

                        OtherKey other ->
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
            case fullStamina model of
                Either.Left _ ->
                    ( model, Cmd.none )

                Either.Right _ ->
                    ( { model | player = Player.increaseStamina model.player }, Cmd.none )

        SelectCraft craft ->
            ( { model | craft = selectCraft model.craft craft }, Cmd.none )


enoughStamina : Model -> Either Model Model
enoughStamina model =
    model.player.stamina
        |> (\(Player.Stamina stamina) ->
                if stamina <= 0 then
                    Either.Left model

                else
                    Either.Right model
           )


fullStamina : Model -> Either Model Model
fullStamina model =
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
    div
        [ style "border" "1px solid black"
        , style "display" "flex"
        , style "margin" "1em"
        ]
        [ div
            sectionStyle
            [ Map.viewLocation playerCoordinate model.worldData
            , viewMoveControls
            , Map.viewMap playerCoordinate model.worldData
            , viewResources locationData
            , viewPlayerItems model.player
            ]
        , div sectionStyle
            [ div []
                [ button
                    [ style "display" "inline-block"
                    , style "width" "10em"
                    , onClick (SelectCraft Alchemy)
                    ]
                    [ text "Alchemy" ]
                , button
                    [ style "display" "inline-block"
                    , style "width" "10em"
                    , onClick (SelectCraft Smith)
                    ]
                    [ text "Smith" ]
                , viewCraft model.craft
                ]
            ]
        ]


sectionStyle : List (Attribute msg)
sectionStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "flex-shrink" "0"
    , style "border" "1px solid black"
    , style "padding" "1em"
    , style "margin" "1em"
    , style "flex" "1"
    ]


viewPlayerItems : Player -> Html Msg
viewPlayerItems player =
    div []
        [ h4 [] [ text "Items:" ]
        , text (Debug.toString player.skills)
        , text (Debug.toString player.stamina)
        , div [] (Dict.values (Dict.map viewResource player.items))
        ]


viewResource : String -> ( Resource, Int ) -> Html msg
viewResource name ( _, amount ) =
    div []
        [ text name
        , text (String.fromInt amount)
        , div [] [ button [] [ text "+" ], button [] [ text "-" ] ]
        ]


viewResources : Maybe Map.LocationData -> Html Msg
viewResources maybeLocationData =
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


toDirection : String -> KeyboardAction
toDirection key =
    case Debug.log "pressed key#" key of
        "ArrowLeft" ->
            Direction West

        "ArrowUp" ->
            Direction North

        "ArrowRight" ->
            Direction East

        "ArrowDown" ->
            Direction South

        " " ->
            OtherKey SpaceBar

        _ ->
            OtherKey (Other key)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Port.keyboardEvent Keyboard
        ]


main : Program () Model Msg
main =
    element
        { init = \flags -> ( initialModel, Random.generate GenerateWorld (Random.int 1 6000) )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
