module Resource exposing
    ( Resource
    , Resources
    , flowers
    , genAmountChance
    , generate
    , generateRandom
    , getAmount
    , getName
    , getRarity
    , mushrooms
    , rockResources
    , takeOne
    , woodResources
    )

import Array exposing (Array)
import Random exposing (Generator)
import SafeUtils exposing (findSafeInDict)


type alias Resources =
    Array Resource


type alias Resource =
    ( Name, Rarity, Amount )


type Name
    = Name String


type Rarity
    = Rarity Float


type Amount
    = Amount Int


woodResources : Resources
woodResources =
    Array.fromList
        [ generate "Oak" 0.1 10
        , generate "Elm" 0.25 10
        , generate "Birch" 0.5 10
        , generate "Willow" 0.3 10
        ]


rockResources : Resources
rockResources =
    Array.fromList
        [ generate "Steel" 0.25 10
        , generate "Bronze" 0.25 10
        , generate "Stone" 0.25 10
        , generate "Gold" 0.0 2
        ]


flowers : Resources
flowers =
    Array.fromList
        [ generate "Buttercup" 0.25 10
        , generate "Daffodil" 0.25 10
        , generate "Tulip" 0.25 10
        , generate "CommonDaisy" 0.25 10
        ]


mushrooms : Resources
mushrooms =
    Array.fromList
        [ generate "Shiitake" 0.25 10
        , generate "Chanterelle" 0.25 10
        , generate "Agaricus" 0.25 10
        , generate "Enoki" 0.25 10
        ]


generateRandom : Resources -> Generator Resource
generateRandom resources =
    Random.int 0 (Array.length resources - 1)
        |> Random.map (\resourceId -> findSafeInDict resourceId resources)


generate : String -> Float -> Int -> Resource
generate name rarity amount =
    ( Name name, Rarity rarity, Amount amount )


takeOne : Resource -> Resource
takeOne ( Name name, Rarity rarity, Amount a ) =
    generate name rarity (a - 1)


getAmount : Resource -> Int
getAmount ( _, _, Amount amount ) =
    amount


getName : Resource -> String
getName ( Name name, _, _ ) =
    name


getRarity : Resource -> Float
getRarity ( _, Rarity rarity, _ ) =
    rarity


genAmountChance : Resource -> Generator ( Int, Float )
genAmountChance ( _, _, Amount maxAmount ) =
    Random.pair
        (Random.int 0 maxAmount)
        (Random.float 0 1)
