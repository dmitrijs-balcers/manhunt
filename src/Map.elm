module Map exposing
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

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (initialSeed, step)
import Set exposing (Set)


type alias World a =
    Array (Array a)


type alias ResourceDict a =
    Dict Int a


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


type alias LocationData =
    ( ( Resource, Action ), Amount )


woodResources : ResourceDict Resource
woodResources =
    Dict.fromList
        [ ( 0, ( "Oak", 0.25, 10 ) )
        , ( 1, ( "Elm", 0.25, 10 ) )
        , ( 2, ( "Birch", 0.25, 10 ) )
        , ( 3, ( "Willow", 0.25, 10 ) )
        ]


rockResources : ResourceDict Resource
rockResources =
    Dict.fromList
        [ ( 0, ( "Steel", 0.25, 10 ) )
        , ( 1, ( "Bronze", 0.25, 10 ) )
        , ( 2, ( "Stone", 0.25, 10 ) )
        , ( 3, ( "Gold", 0.25, 10 ) )
        ]


flowers : ResourceDict Resource
flowers =
    Dict.fromList
        [ ( 0, ( "Buttercup", 0.25, 10 ) )
        , ( 1, ( "Daffodil", 0.25, 10 ) )
        , ( 2, ( "Tulip", 0.25, 10 ) )
        , ( 3, ( "CommonDaisy", 0.25, 10 ) )
        ]


mushrooms : ResourceDict Resource
mushrooms =
    Dict.fromList
        [ ( 0, ( "Shiitake", 0.25, 10 ) )
        , ( 1, ( "Chanterelle", 0.25, 10 ) )
        , ( 2, ( "Agaricus", 0.25, 10 ) )
        , ( 3, ( "Enoki", 0.25, 10 ) )
        ]


actions : Dict Int String
actions =
    Dict.fromList
        [ ( 0, "Pluck" )
        , ( 1, "Mine" )
        , ( 2, "Chop" )
        ]


type Action
    = Pluck
    | Mine
    | Chop


type alias LandscapeResources =
    List ( Action, ResourceDict Resource )


type alias LandscapeName =
    String


landscapes : Dict LandscapeId ( LandscapeName, LandscapeResources )
landscapes =
    Dict.fromList
        [ ( 0, ( "Field", [ ( Pluck, flowers ) ] ) )
        , ( 1, ( "Rock", [ ( Mine, rockResources ) ] ) )
        , ( 2, ( "Forest", [ ( Chop, woodResources ), ( Pluck, mushrooms ) ] ) )
        ]


findSafeInDict : Int -> ResourceDict a -> a
findSafeInDict id dict =
    case Dict.get id dict of
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
        landscapeValue : LandscapeResources
        landscapeValue =
            findSafeInDict landscapeId landscapes
                |> Tuple.second

        randomLandscapeResourceTypeId : Int
        randomLandscapeResourceTypeId =
            step (Random.int 0 (List.length landscapeValue - 1)) (initialSeed seed) |> Tuple.first

        landscapeRandomResource : ( Action, ResourceDict Resource )
        landscapeRandomResource =
            findSafeInList randomLandscapeResourceTypeId landscapeValue

        landscapeResources : ResourceDict Resource
        landscapeResources =
            Tuple.second landscapeRandomResource

        landscapeAction : Action
        landscapeAction =
            Tuple.first landscapeRandomResource

        resourceId : Int
        resourceId =
            step (Random.int 0 (Dict.size landscapeResources - 1)) (initialSeed seed)
                |> Tuple.first

        resource : Resource
        resource =
            findSafeInDict resourceId landscapeResources

        ( resourceName, chanceToFind, maxAmount ) =
            resource

        amount : Amount
        amount =
            Tuple.first (step (Random.int 0 maxAmount) (initialSeed seed))
    in
    if amount > 0 then
        Just ( ( resource, landscapeAction ), amount )

    else
        Nothing


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


worldMap : World LandscapeId
worldMap =
    listToArray
        [ [ 0, 0, 2, 0, 0 ]
        , [ 0, 1, 1, 1, 0 ]
        , [ 2, 1, 2, 1, 2 ]
        , [ 0, 1, 1, 1, 0 ]
        , [ 0, 0, 2, 0, 0 ]
        ]


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


coordinateOnMap : ( Int, Int ) -> ( Int, Int )
coordinateOnMap ( lat, lon ) =
    ( mapHeight - lat, mapHeight - lon )


getLandscape : ( Int, Int ) -> Maybe LandscapeId
getLandscape position =
    getItemFrom2dArray (coordinateOnMap position) worldMap


mapHeight : Int
mapHeight =
    (Array.length worldMap - 1) // 2


generateMiniMap : ( Int, Int ) -> Int -> World a -> World a
generateMiniMap ( lat, lon ) visibility arr =
    let
        ( latRange, ( lonFrom, lonTo ) ) =
            ( ( lat - visibility, lat + 1 + visibility )
            , ( lon - visibility, lon + visibility )
            )
    in
    Array.fromList
        [ cutRow latRange (arrOrEmpty (Array.get lonFrom arr))
        , cutRow latRange (arrOrEmpty (Array.get lon arr))
        , cutRow latRange (arrOrEmpty (Array.get lonTo arr))
        ]


cutRow : ( Int, Int ) -> Array a -> Array a
cutRow ( latFrom, latTo ) row =
    Array.slice latFrom latTo row


arrOrEmpty : Maybe (Array a) -> Array a
arrOrEmpty mArr =
    Maybe.withDefault Array.empty mArr


landscapeToString : LandscapeId -> String
landscapeToString landscape =
    case Dict.get landscape landscapes of
        Just l ->
            Tuple.first l

        Nothing ->
            Debug.todo "Not landscape found :/"



-- VIEW


viewMap : ( Int, Int ) -> Html msg
viewMap ( lat, lon ) =
    div [] (Array.toList (Array.map viewRow (generateMiniMap (coordinateOnMap ( lat, lon )) (1 * 2 - 1) worldMap)))


viewRow : Array LandscapeId -> Html msg
viewRow row =
    div []
        [ text
            ("["
                ++ String.join "," (Array.toList (Array.map landscapeToString row))
                ++ "]"
            )
        ]


viewLocation : ( Int, Int ) -> Html msg
viewLocation pos =
    let
        coordinateToString : ( Int, Int ) -> String
        coordinateToString ( lat, lon ) =
            "[" ++ String.fromInt lat ++ "/" ++ String.fromInt lon ++ "]"
    in
    case getItemFrom2dArray (coordinateOnMap pos) worldMap of
        Just landscape ->
            text ("Location: " ++ landscapeToString landscape ++ " " ++ coordinateToString pos)

        Nothing ->
            text ("N/A " ++ coordinateToString pos)


viewLocationAction : (Action -> msg) -> Maybe LocationData -> Html msg
viewLocationAction msg maybeLocationData =
    case maybeLocationData of
        Just locationData ->
            div []
                [ button
                    [ onClick (msg (Tuple.second (Tuple.first locationData))) ]
                    [ text (stringifyAction (Tuple.second (Tuple.first locationData))) ]
                ]

        Nothing ->
            text ""


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
    case Dict.get landscape landscapes of
        Just l ->
            Tuple.first l

        Nothing ->
            Debug.todo "No Landscape Found"
