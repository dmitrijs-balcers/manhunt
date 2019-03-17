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
        [ ( Resource ( Name "Oak", Rarity 0.1 ), Amount 10 )
        , ( Resource ( Name "Elm", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Birch", Rarity 0.5 ), Amount 10 )
        , ( Resource ( Name "Willow", Rarity 0.3 ), Amount 10 )
        ]


rockResources : Resources
rockResources =
    Array.fromList
        [ ( Resource ( Name "Steel", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Bronze", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Stone", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Gold", Rarity 0.001 ), Amount 2 )
        ]


flowers : Resources
flowers =
    Array.fromList
        [ ( Resource ( Name "Buttercup", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Daffodil", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Tulip", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "CommonDaisy", Rarity 0.25 ), Amount 10 )
        ]


mushrooms : Resources
mushrooms =
    Array.fromList
        [ ( Resource ( Name "Shiitake", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Chanterelle", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Agaricus", Rarity 0.25 ), Amount 10 )
        , ( Resource ( Name "Enoki", Rarity 0.25 ), Amount 10 )
        ]
