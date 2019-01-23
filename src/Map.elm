module Map exposing
    ( Action(..)
    , Amount(..)
    , Direction(..)
    , Gather(..)
    , Height
    , LocationData
    , Rarity(..)
    , Resource(..)
    , ResourceName(..)
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


type Direction
    = North
    | South
    | West
    | East


{-| encapsulate this using function <https://egghead.io/lessons/elm-use-single-constructor-union-types-in-elm-to-prevent-invalid-data>
-}
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


type alias
    Resources
    -- shitty, but array (resource, maxAmount)
    =
    Array ( Resource, Amount )


type Resource
    = Resource ( ResourceName, Rarity )


type alias LocationData =
    ( ( Resource, Gather ), Amount )


type Action
    = Move Direction
    | Gather Gather


type Gather
    = Pluck
    | Mine
    | Chop


type LandscapeResource
    = LandscapeResource ( Gather, Resources )


type LandscapeResources
    = LandscapeResources (List LandscapeResource)


type LandscapeName
    = LandscapeName String


type alias Landscape =
    ( LandscapeName, LandscapeResources )


type Landscapes
    = Landscapes (Array Landscape)


type Visibility
    = Visibility Int


woodResources : Resources
woodResources =
    Array.fromList
        [ ( Resource ( ResourceName "Oak", Rarity 0.1 ), Amount 10 )
        , ( Resource ( ResourceName "Elm", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Birch", Rarity 0.5 ), Amount 10 )
        , ( Resource ( ResourceName "Willow", Rarity 0.3 ), Amount 10 )
        ]


rockResources : Resources
rockResources =
    Array.fromList
        [ ( Resource ( ResourceName "Steel", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Bronze", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Stone", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Gold", Rarity 0.001 ), Amount 2 )
        ]


flowers : Resources
flowers =
    Array.fromList
        [ ( Resource ( ResourceName "Buttercup", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Daffodil", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Tulip", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "CommonDaisy", Rarity 0.25 ), Amount 10 )
        ]


mushrooms : Resources
mushrooms =
    Array.fromList
        [ ( Resource ( ResourceName "Shiitake", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Chanterelle", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Agaricus", Rarity 0.25 ), Amount 10 )
        , ( Resource ( ResourceName "Enoki", Rarity 0.25 ), Amount 10 )
        ]


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


generateLocationData : LandscapeId -> Generator (Maybe LocationData)
generateLocationData (LandscapeId landscapeId) =
    let
        (Landscapes l) =
            landscapes

        ( _, landscapeResources ) =
            findSafeInDict landscapeId l
    in
    genRandomLandscapeResource landscapeResources
        |> Random.andThen
            (\(LandscapeResource ( resourceAction, resources )) ->
                genRandomResource resources
                    |> Random.andThen
                        (\( Resource ( ResourceName resourceName, Rarity rarity ), maxAmount ) ->
                            genAmountChance maxAmount
                                |> Random.map
                                    (\( amount, luck ) ->
                                        if amount > 0 && succeed luck rarity then
                                            let
                                                r =
                                                    ( Resource ( ResourceName resourceName, Rarity rarity )
                                                    , resourceAction
                                                    )
                                            in
                                            Just ( r, Amount amount )

                                        else
                                            Debug.log
                                                (stringifyLandscapeFailure resourceName amount luck)
                                                Nothing
                                    )
                        )
            )


genRandomLandscapeResource : LandscapeResources -> Generator LandscapeResource
genRandomLandscapeResource (LandscapeResources landscapeResources) =
    Random.int 0 (List.length landscapeResources - 1)
        |> Random.map (\id -> findSafeInList id landscapeResources)


genRandomResource : Resources -> Generator ( Resource, Amount )
genRandomResource resources =
    Random.int 0 (Array.length resources - 1)
        |> Random.map (\resourceId -> findSafeInDict resourceId resources)


genAmountChance : Amount -> Generator ( Int, Float )
genAmountChance (Amount maxAmount) =
    Random.pair
        (Random.int 0 maxAmount)
        (Random.float 0 1)


succeed : Float -> Float -> Bool
succeed rolledLuck chanceNeededToSucceed =
    rolledLuck > chanceNeededToSucceed


stringifyLandscapeFailure : String -> Int -> Float -> String
stringifyLandscapeFailure resourceName amount luck =
    "Didn't find "
        ++ resourceName
        ++ "with amount#"
        ++ String.fromInt amount
        ++ " and luck#"
        ++ String.fromFloat luck


stringifyLocationData : LocationData -> String
stringifyLocationData locationData =
    let
        ( ( Resource ( ResourceName name, _ ), action ), Amount amount ) =
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


getLandscape : World Height -> ( Int, Int ) -> Maybe LandscapeId
getLandscape worldMap position =
    case getItemFrom2dArray (coordinateOnMap position (Array.length worldMap)) worldMap of
        Just height ->
            Just (heightToLandscapeId height)

        Nothing ->
            Nothing


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
        coordinate =
            coordinateOnMap ( lat, lon ) (Array.length worldMap)

        visibility =
            Visibility 5
    in
    generateMiniMap coordinate visibility worldMap
        |> Array.indexedMap (viewRow 5)
        |> Array.toList
        |> div []


generateMiniMap : ( Int, Int ) -> Visibility -> World a -> World a
generateMiniMap ( lat, lon ) (Visibility visibility) world =
    let
        ( latRange, ( lonFrom, lonTo ) ) =
            ( ( lat - visibility, lat + 1 + visibility )
            , ( lon - visibility, lon + 1 + visibility )
            )

        arrayOfHeight =
            Array.initialize (lonTo - lonFrom) (\n -> lonFrom + n)
    in
    Array.map (mapRow latRange world) arrayOfHeight


mapRow : ( Int, Int ) -> World a -> Int -> Array a
mapRow latRange world rowIndex =
    cutRow latRange (arrOrEmpty (Array.get rowIndex world))


cutRow : ( Int, Int ) -> Array a -> Array a
cutRow ( latFrom, latTo ) row =
    Array.slice latFrom latTo row


viewRow : Int -> Int -> Array Height -> Html msg
viewRow middle rowIndex row =
    div
        [ style "display" "block"
        , style "height" "32px"
        ]
        (Array.toList row
            |> List.indexedMap (viewColumn { middle = middle, rowIndex = rowIndex })
        )


viewColumn : { middle : Int, rowIndex : Int } -> Int -> Height -> Html msg
viewColumn { middle, rowIndex } columnIndex height =
    let
        inversionStyle : Html.Attribute msg
        inversionStyle =
            if middle == rowIndex && middle == columnIndex then
                style "filter" "invert(100%)"

            else
                style "filter" "invert(0%)"
    in
    div
        [ style "display" "inline-block"
        , inversionStyle
        ]
        [ landscapeToImg height ]


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
            [ onClick (msg (Gather (Tuple.second (Tuple.first locationData)))) ]
            [ text (stringifyAction (Tuple.second (Tuple.first locationData))) ]
        ]


stringifyAction : Gather -> String
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
