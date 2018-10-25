module Map exposing
    ( Action
    , Height
    , LocationData
    , World
    , generateLocationData
    , generateWorld
    , getLandscape
    , landscapeToString
    , stringifyLocationData
    , viewLocation
    , viewLocationAction
    , viewMap
    )

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed, step)
import Set exposing (Set)
import Simplex


type alias World a =
    Array (Array a)


type alias ResourceDict a =
    Array a


type alias Resources a =
    Dict Int (ResourceDict a)


type alias LandscapeId =
    Int


type alias ResourceName =
    String


type alias Rarity =
    Float


type alias MaxAmount =
    Amount


type alias Resource =
    ( String, Rarity, MaxAmount )


type alias Amount =
    Int


type alias Height =
    Float


type alias Size =
    Int


type alias LocationData =
    ( ( Resource, Action ), Amount )


woodResources : ResourceDict Resource
woodResources =
    Array.fromList
        [ ( "Oak", 0.1, 10 )
        , ( "Elm", 0.25, 10 )
        , ( "Birch", 0.5, 10 )
        , ( "Willow", 0.3, 10 )
        ]


rockResources : ResourceDict Resource
rockResources =
    Array.fromList
        [ ( "Steel", 0.25, 10 )
        , ( "Bronze", 0.25, 10 )
        , ( "Stone", 0.25, 10 )
        , ( "Gold", 0.001, 2 )
        ]


flowers : ResourceDict Resource
flowers =
    Array.fromList
        [ ( "Buttercup", 0.25, 10 )
        , ( "Daffodil", 0.25, 10 )
        , ( "Tulip", 0.25, 10 )
        , ( "CommonDaisy", 0.25, 10 )
        ]


mushrooms : ResourceDict Resource
mushrooms =
    Array.fromList
        [ ( "Shiitake", 0.25, 10 )
        , ( "Chanterelle", 0.25, 10 )
        , ( "Agaricus", 0.25, 10 )
        , ( "Enoki", 0.25, 10 )
        ]


generateWorld : Size -> Seed -> World Height
generateWorld size seed =
    let
        smoothFactor =
            0.01

        permutationTable =
            Simplex.permutationTable seed
    in
    Array.initialize size
        (\x ->
            Array.initialize size
                (\y ->
                    let
                        floatPoint =
                            ( toFloat x * smoothFactor, toFloat y * smoothFactor )
                    in
                    Simplex.simplex2D permutationTable floatPoint
                )
        )


type Action
    = Pluck
    | Mine
    | Chop


type alias LandscapeResources =
    List ( Action, ResourceDict Resource )


type alias LandscapeName =
    String


landscapes : Array ( LandscapeName, LandscapeResources )
landscapes =
    Array.fromList
        [ ( "Field", [ ( Pluck, flowers ) ] )
        , ( "Forest", [ ( Chop, woodResources ), ( Pluck, mushrooms ) ] )
        , ( "Hill", [ ( Mine, rockResources ) ] )
        ]


findSafeInDict : Int -> ResourceDict a -> a
findSafeInDict id dict =
    case Array.get id dict of
        Just res ->
            res

        Nothing ->
            Debug.todo ("Didn't find " ++ String.fromInt id ++ " in " ++ Debug.toString dict)


findSafeInList : Int -> List a -> a
findSafeInList id list =
    case Array.fromList list |> Array.get id of
        Just el ->
            el

        Nothing ->
            Debug.todo ("Didn't find " ++ String.fromInt id ++ " in " ++ Debug.toString list)


generateLocationData : Int -> LandscapeId -> Maybe LocationData
generateLocationData seed landscapeId =
    let
        ( _, landscapeValue ) =
            findSafeInDict landscapeId landscapes

        ( randomLandscapeResourceTypeId, resourceTypeSeed ) =
            step (Random.int 0 (List.length landscapeValue - 1)) (initialSeed seed)

        ( landscapeAction, landscapeResources ) =
            findSafeInList randomLandscapeResourceTypeId landscapeValue

        ( resourceId, resourceSeed ) =
            step (Random.int 0 (Array.length landscapeResources - 1)) resourceTypeSeed

        resource : Resource
        resource =
            findSafeInDict resourceId landscapeResources

        ( resourceName, chanceToFind, maxAmount ) =
            resource

        ( amount, amountSeed ) =
            step (Random.int 0 maxAmount) resourceSeed

        ( luck, luckSeed ) =
            rollDice amountSeed
    in
    if amount > 0 && succeed luck chanceToFind then
        Just ( ( resource, landscapeAction ), amount )

    else
        Debug.log ("Didn't find " ++ Debug.toString resource ++ "with amount#" ++ String.fromInt amount ++ " and luck#" ++ String.fromFloat luck) Nothing


rollDice : Seed -> ( Float, Seed )
rollDice seed =
    step (Random.float 0 1) seed


succeed : Float -> Float -> Bool
succeed rolledLuck chanceNeededToSucceed =
    rolledLuck > chanceNeededToSucceed


stringifyLocationData : LocationData -> String
stringifyLocationData locationData =
    let
        resource : Resource
        resource =
            locationData |> Tuple.first |> Tuple.first

        ( name, _, _ ) =
            resource
    in
    name ++ " " ++ (Tuple.second locationData |> String.fromInt)


getItemFrom2dArray : ( Int, Int ) -> World a -> Maybe a
getItemFrom2dArray ( lat, lon ) map =
    Array.get lat (Maybe.withDefault Array.empty (Array.get lon map))


listToArray : List (List a) -> World a
listToArray mapList =
    let
        mapper : List a -> World a -> World a
        mapper resource mapArray =
            Array.push (Array.fromList resource) mapArray
    in
    List.foldl mapper (Array.fromList []) mapList


coordinateOnMap : ( Int, Int ) -> Int -> ( Int, Int )
coordinateOnMap ( lat, lon ) mapSize =
    let
        toHalf =
            mapSize // 2 - 1
    in
    ( toHalf - lat, toHalf - lon )


getLandscape : ( Int, Int ) -> World Height -> Maybe LandscapeId
getLandscape position worldMap =
    case getItemFrom2dArray (coordinateOnMap position (Array.length worldMap)) worldMap of
        Just height ->
            Just (heightToLandscapeId height)

        Nothing ->
            Nothing


generateMiniMap : ( Int, Int ) -> Int -> World a -> World a
generateMiniMap ( lat, lon ) visibility arr =
    let
        ( latRange, ( lonFrom, lonTo ) ) =
            ( ( lat - visibility, lat + 1 + visibility )
            , ( lon - visibility, lon + visibility )
            )

        arrayOfHeight =
            Array.initialize (lonTo - lonFrom) (\n -> lonFrom + n)
    in
    Array.map (\n -> cutRow latRange (arrOrEmpty (Array.get n arr))) arrayOfHeight


cutRow : ( Int, Int ) -> Array a -> Array a
cutRow ( latFrom, latTo ) row =
    Array.slice latFrom latTo row


arrOrEmpty : Maybe (Array a) -> Array a
arrOrEmpty mArr =
    Maybe.withDefault Array.empty mArr


landscapeToString : Height -> String
landscapeToString height =
    let
        landscapeId =
            heightToLandscapeId height
    in
    case Array.get landscapeId landscapes of
        Just l ->
            Tuple.first l

        Nothing ->
            Debug.todo "Not landscape found :/"


heightToLandscapeId : Height -> LandscapeId
heightToLandscapeId height =
    if height < (0.66 - 1) then
        0

    else if height > (0.66 - 1) && height < 0.36 then
        1

    else
        2



-- VIEW


viewMap : ( Int, Int ) -> World Height -> Html msg
viewMap ( lat, lon ) worldMap =
    let
        mapCoord =
            coordinateOnMap ( lat, lon ) (Array.length worldMap)
    in
    div [] (Array.toList (Array.map viewRow (generateMiniMap mapCoord 10 worldMap)))


viewRow : Array Height -> Html msg
viewRow row =
    div
        [ style "display" "block"
        , style "height" "32px"
        ]
        (Array.toList row
            |> List.map
                (\h ->
                    div
                        [ style "display" "inline-block" ]
                        [ landscapeToImg h ]
                )
        )


landscapeToImg : Height -> Html msg
landscapeToImg height =
    let
        landscapeId =
            heightToLandscapeId height
    in
    case landscapeId of
        0 ->
            img [ src "../assets/field.png" ] []

        1 ->
            img [ src "../assets/forest.png" ] []

        _ ->
            img [ src "../assets/hill.png" ] []


viewLocation : ( Int, Int ) -> World Height -> Html msg
viewLocation pos worldMap =
    let
        coordinateToString : ( Int, Int ) -> String
        coordinateToString ( lat, lon ) =
            "[" ++ String.fromInt lat ++ "/" ++ String.fromInt lon ++ "]"
    in
    case getItemFrom2dArray (coordinateOnMap pos (Array.length worldMap)) worldMap of
        Just landscape ->
            div
                [ style "color" "red" ]
                [ text ("Location: " ++ landscapeToString landscape ++ " " ++ coordinateToString pos) ]

        Nothing ->
            text ("N/A " ++ coordinateToString pos)


viewLocationAction : (Action -> msg) -> LocationData -> Html msg
viewLocationAction msg locationData =
    div []
        [ button
            [ onClick (msg (Tuple.second (Tuple.first locationData))) ]
            [ text (stringifyAction (Tuple.second (Tuple.first locationData))) ]
        ]


stringifyAction : Action -> String
stringifyAction action =
    case action of
        Pluck ->
            "Pluck"

        Mine ->
            "Mine"

        Chop ->
            "Chop"


stringifyResource : LandscapeId -> String
stringifyResource landscape =
    case Array.get landscape landscapes of
        Just l ->
            Tuple.first l

        Nothing ->
            Debug.todo "No Landscape Found"
