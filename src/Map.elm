module Map exposing
    ( Action(..)
    , Direction(..)
    , Gather(..)
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
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Random exposing (Generator, Seed)
import Resource exposing (Resource, Resources)
import SafeUtils exposing (findSafeInDict, findSafeInList)
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


type Height
    = Height Float


type Size
    = Size Int


type alias World a =
    Array (Array a)


type alias LocationData =
    ( Resource, Gather )


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
                    [ LandscapeResource ( Pluck, Resource.flowers ) ]
              )
            , ( LandscapeName "Forest"
              , LandscapeResources
                    [ LandscapeResource ( Chop, Resource.woodResources )
                    , LandscapeResource ( Pluck, Resource.mushrooms )
                    ]
              )
            , ( LandscapeName "Hill"
              , LandscapeResources
                    [ LandscapeResource ( Mine, Resource.rockResources ) ]
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
                Resource.generateRandom resources
                    |> Random.andThen
                        (\resource ->
                            let
                                rarity =
                                    Resource.getRarity resource

                                name =
                                    Resource.getResourceName resource
                            in
                            Resource.genAmountChance resource
                                |> Random.map
                                    (\( amount, luck ) ->
                                        if amount > 0 && succeed luck rarity then
                                            Just ( Resource.generate resource amount, resourceAction )

                                        else
                                            Debug.log
                                                (stringifyLandscapeFailure name amount luck)
                                                Nothing
                                    )
                        )
            )


genRandomLandscapeResource : LandscapeResources -> Generator LandscapeResource
genRandomLandscapeResource (LandscapeResources landscapeResources) =
    Random.int 0 (List.length landscapeResources - 1)
        |> Random.map (\id -> findSafeInList id landscapeResources)


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
stringifyLocationData ( resource, _ ) =
    ( Resource.getResourceName resource, Resource.getAmount resource )
        |> (\( name, amount ) -> name ++ " " ++ String.fromInt amount)


getItemFrom2dArray : ( Int, Int ) -> World a -> Maybe a
getItemFrom2dArray ( lat, lon ) map =
    Array.get lat (Maybe.withDefault Array.empty (Array.get lon map))


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
        |> div [ style "display" "inline-flex", style "flex-direction" "column" ]


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
        [ style "display" "inline-block"
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
        , style "width" "32px"
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
                [ style "color" "red"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ text ("Location: " ++ landscapeToString landscape ++ " " ++ coordinateToString pos) ]

        Nothing ->
            text ("N/A " ++ coordinateToString pos)


viewLocationAction : (Action -> msg) -> LocationData -> Html msg
viewLocationAction msg locationData =
    div []
        [ button
            [ onClick (msg (Gather (Tuple.second locationData))) ]
            [ text (stringifyAction (Tuple.second locationData)) ]
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
