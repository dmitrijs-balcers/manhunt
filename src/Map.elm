module Map exposing
    ( Action
    , Amount(..)
    , Height
    , LocationData
    , Size(..)
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
import Random exposing (Generator, Seed, initialSeed, step)
import SafeUtils exposing (findSafeInDict, findSafeInList)
import Set exposing (Set)
import Simplex


type LandscapeId
    = LandscapeId Int


type ResourceName
    = ResourceName String


type Rarity
    = Rarity Float


type Amount
    = Amount Int


type Height
    = Height Float


type Size
    = Size Int


type alias World a =
    Array (Array a)


type Resources a
    = Resources (Array a)


type alias Resource =
    ( ResourceName, Rarity, Amount )


type alias LocationData =
    ( ( Resource, Action ), Amount )


woodResources : Resources Resource
woodResources =
    Resources
        (Array.fromList
            [ ( ResourceName "Oak", Rarity 0.1, Amount 10 )
            , ( ResourceName "Elm", Rarity 0.25, Amount 10 )
            , ( ResourceName "Birch", Rarity 0.5, Amount 10 )
            , ( ResourceName "Willow", Rarity 0.3, Amount 10 )
            ]
        )


rockResources : Resources Resource
rockResources =
    Resources
        (Array.fromList
            [ ( ResourceName "Steel", Rarity 0.25, Amount 10 )
            , ( ResourceName "Bronze", Rarity 0.25, Amount 10 )
            , ( ResourceName "Stone", Rarity 0.25, Amount 10 )
            , ( ResourceName "Gold", Rarity 0.001, Amount 2 )
            ]
        )


flowers : Resources Resource
flowers =
    Resources
        (Array.fromList
            [ ( ResourceName "Buttercup", Rarity 0.25, Amount 10 )
            , ( ResourceName "Daffodil", Rarity 0.25, Amount 10 )
            , ( ResourceName "Tulip", Rarity 0.25, Amount 10 )
            , ( ResourceName "CommonDaisy", Rarity 0.25, Amount 10 )
            ]
        )


mushrooms : Resources Resource
mushrooms =
    Resources
        (Array.fromList
            [ ( ResourceName "Shiitake", Rarity 0.25, Amount 10 )
            , ( ResourceName "Chanterelle", Rarity 0.25, Amount 10 )
            , ( ResourceName "Agaricus", Rarity 0.25, Amount 10 )
            , ( ResourceName "Enoki", Rarity 0.25, Amount 10 )
            ]
        )


generateWorld : Size -> Seed -> World Height
generateWorld (Size size) seed =
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
                    Height (Simplex.simplex2D permutationTable floatPoint)
                )
        )


type Action
    = Pluck
    | Mine
    | Chop


type LandscapeResource
    = LandscapeResource ( Action, Resources Resource )


type LandscapeResources
    = LandscapeResources (List LandscapeResource)


type LandscapeName
    = LandscapeName String


type alias Landscape =
    ( LandscapeName, LandscapeResources )


type Landscapes
    = Landscapes (Array Landscape)


landscapes : Landscapes
landscapes =
    Landscapes
        (Array.fromList
            [ ( LandscapeName "Field"
              , LandscapeResources
                    [ LandscapeResource ( Pluck, flowers ) ]
              )
            , ( LandscapeName "Forest"
              , LandscapeResources
                    [ LandscapeResource ( Chop, woodResources )
                    , LandscapeResource ( Pluck, mushrooms )
                    ]
              )
            , ( LandscapeName "Hill"
              , LandscapeResources
                    [ LandscapeResource ( Mine, rockResources ) ]
              )
            ]
        )


generateLocationData : Seed -> LandscapeId -> Maybe LocationData
generateLocationData seed (LandscapeId landscapeId) =
    let
        (Landscapes l) =
            landscapes

        ( _, LandscapeResources landscapeResources ) =
            findSafeInDict landscapeId l

        ( randomLandscapeResourceTypeId, resourceTypeSeed ) =
            step (Random.int 0 (List.length landscapeResources - 1)) seed

        (LandscapeResource ( resourceAction, Resources resources )) =
            findSafeInList randomLandscapeResourceTypeId landscapeResources

        ( resourceId, resourceSeed ) =
            step (Random.int 0 (Array.length resources - 1)) resourceTypeSeed

        resource : Resource
        resource =
            findSafeInDict resourceId resources

        ( resourceName, Rarity rarity, Amount maxAmount ) =
            resource

        ( amount, amountSeed ) =
            step (Random.int 0 maxAmount) resourceSeed

        ( luck, luckSeed ) =
            rollDice amountSeed
    in
    if amount > 0 && succeed luck rarity then
        Just ( ( resource, resourceAction ), Amount amount )

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
        ( ( ( ResourceName name, _, _ ), action ), Amount amount ) =
            locationData
    in
    name ++ " " ++ String.fromInt amount


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
        (LandscapeId landscapeId) =
            heightToLandscapeId height

        (Landscapes l) =
            landscapes
    in
    case Array.get landscapeId l of
        Just ( LandscapeName name, res ) ->
            name

        Nothing ->
            Debug.todo "Not landscape found :/"


heightToLandscapeId : Height -> LandscapeId
heightToLandscapeId (Height height) =
    if height < (0.66 - 1) then
        LandscapeId 0

    else if height > (0.66 - 1) && height < 0.36 then
        LandscapeId 1

    else
        LandscapeId 2



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
        (LandscapeId landscapeId) =
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
stringifyResource (LandscapeId landscape) =
    let
        (Landscapes l) =
            landscapes
    in
    case Array.get landscape l of
        Just ( LandscapeName name, _ ) ->
            name

        Nothing ->
            Debug.todo "No Landscape Found"
