module Resource exposing (Amount(..), Name(..), Rarity(..), Resource(..), Resources, flowers, mushrooms, rockResources, woodResources)

import Array exposing (Array)


type alias Resources =
    Array ( Resource, Amount )


type Resource
    = Resource ( Name, Rarity )


type Name
    = Name String


type Rarity
    = Rarity Float


type Amount
    = Amount Int


woodResources : Resources
woodResources =
    Array.fromList
        [ generateResource "Oak" 0.1 10
        , generateResource "Elm" 0.25 10
        , generateResource "Birch" 0.5 10
        , generateResource "Willow" 0.3 10
        ]


rockResources : Resources
rockResources =
    Array.fromList
        [ generateResource "Steel" 0.25 10
        , generateResource "Bronze" 0.25 10
        , generateResource "Stone" 0.25 10
        , generateResource "Gold" 0.0 2
        ]


flowers : Resources
flowers =
    Array.fromList
        [ generateResource "Buttercup" 0.25 10
        , generateResource "Daffodil" 0.25 10
        , generateResource "Tulip" 0.25 10
        , generateResource "CommonDaisy" 0.25 10
        ]


mushrooms : Resources
mushrooms =
    Array.fromList
        [ generateResource "Shiitake" 0.25 10
        , generateResource "Chanterelle" 0.25 10
        , generateResource "Agaricus" 0.25 10
        , generateResource "Enoki" 0.25 10
        ]


generateResource : String -> Float -> Int -> ( Resource, Amount )
generateResource name rarity amount =
    ( Resource ( Name name, Rarity rarity ), Amount amount )
