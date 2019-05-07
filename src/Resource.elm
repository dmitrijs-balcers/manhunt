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


type Resource
    = Resource String Float Int


woodResources : Resources
woodResources =
    Array.fromList
        [ Resource "Oak" 0.1 10
        , Resource "Elm" 0.25 10
        , Resource "Birch" 0.5 10
        , Resource "Willow" 0.3 10
        ]


rockResources : Resources
rockResources =
    Array.fromList
        [ Resource "Steel" 0.25 10
        , Resource "Bronze" 0.25 10
        , Resource "Stone" 0.25 10
        , Resource "Gold" 0.0 2
        ]


flowers : Resources
flowers =
    Array.fromList
        [ Resource "Buttercup" 0.25 10
        , Resource "Daffodil" 0.25 10
        , Resource "Tulip" 0.25 10
        , Resource "CommonDaisy" 0.25 10
        ]


mushrooms : Resources
mushrooms =
    Array.fromList
        [ Resource "Shiitake" 0.25 10
        , Resource "Chanterelle" 0.25 10
        , Resource "Agaricus" 0.25 10
        , Resource "Enoki" 0.25 10
        ]


generateRandom : Resources -> Generator Resource
generateRandom resources =
    Random.int 0 (Array.length resources - 1)
        |> Random.map (\resourceId -> findSafeInDict resourceId resources)


generate : String -> Float -> Int -> Resource
generate name rarity amount =
    Resource name rarity amount


takeOne : Resource -> Resource
takeOne (Resource name rarity amount) =
    generate name rarity (amount - 1)


getAmount : Resource -> Int
getAmount (Resource _ _ amount) =
    amount


getName : Resource -> String
getName (Resource name _ _) =
    name


getRarity : Resource -> Float
getRarity (Resource _ rarity _) =
    rarity


genAmountChance : Resource -> Generator ( Int, Float )
genAmountChance resource =
    Random.pair
        (Random.int 0 (getAmount resource))
        (Random.float 0 1)
