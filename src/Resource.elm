module Resource exposing
    ( Effect
    , Flower
    , Intensity
    , Mushroom
    , Resource
    , Resources
    , Rock
    , Wood
    , flowerEffect
    , flowers
    , genAmountChance
    , generate
    , generateRandom
    , getAmount
    , getRarity
    , getResourceName
    , isAlchemyResource
    , isSmithingResource
    , mushrooms
    , rockResources
    , takeOne
    , woodResources
    , woodToString
    )

import Array exposing (Array)
import Random exposing (Generator)
import SafeUtils exposing (findSafeInDict)


type alias Resources =
    Array Resource


type Resource
    = Wood Wood Float Int
    | Rock Rock Float Int
    | Flower Flower Float Int
    | Mushroom Mushroom Float Int


type Wood
    = Oak
    | Elm
    | Birch
    | Willow


isAlchemyResource : Resource -> Maybe Resource
isAlchemyResource resource =
    case resource of
        Flower _ _ _ ->
            Just resource

        Mushroom _ _ _ ->
            Just resource

        _ ->
            Nothing


isSmithingResource : Resource -> Maybe Resource
isSmithingResource resource =
    case resource of
        Rock _ _ _ ->
            Just resource

        _ ->
            Nothing


woodResources : Resources
woodResources =
    Array.fromList
        [ Wood Oak 0.1 10
        , Wood Elm 0.25 10
        , Wood Birch 0.5 10
        , Wood Willow 0.3 10
        ]


type Rock
    = Steel
    | Bronze
    | Stone
    | Gold


rockResources : Resources
rockResources =
    Array.fromList
        [ Rock Steel 0.25 10
        , Rock Bronze 0.25 10
        , Rock Stone 0.25 10
        , Rock Gold 0.25 2
        ]


type Effect
    = Healing
    | Poison
    | Strength


type Intensity
    = Intensity Int


type Flower
    = Buttercup
    | Daffodil
    | Tulip
    | CommonDaisy
    | Rose


flowers : Resources
flowers =
    Array.fromList
        [ Flower Buttercup 0.25 10
        , Flower Daffodil 0.25 10
        , Flower Tulip 0.25 10
        , Flower CommonDaisy 0.13 10
        , Flower Rose 0.13 10
        ]


flowerEffect : Flower -> ( Effect, Intensity )
flowerEffect flower =
    case flower of
        Buttercup ->
            ( Healing, Intensity 1 )

        Daffodil ->
            ( Poison, Intensity 1 )

        Tulip ->
            ( Strength, Intensity 1 )

        CommonDaisy ->
            ( Healing, Intensity 2 )

        Rose ->
            ( Strength, Intensity 2 )


type Mushroom
    = Shiitake
    | Chanterelle
    | Agaricus
    | Enoki


mushrooms : Resources
mushrooms =
    Array.fromList
        [ Mushroom Shiitake 0.25 10
        , Mushroom Chanterelle 0.25 10
        , Mushroom Agaricus 0.25 10
        , Mushroom Enoki 0.25 10
        ]


generateRandom : Resources -> Generator Resource
generateRandom resources =
    Random.int 0 (Array.length resources - 1)
        |> Random.map (\resourceId -> findSafeInDict resourceId resources)


generate : Resource -> Int -> Resource
generate resource amount =
    case resource of
        Wood name r _ ->
            Wood name r amount

        Rock name r _ ->
            Rock name r amount

        Flower name r _ ->
            Flower name r amount

        Mushroom name r _ ->
            Mushroom name r amount


takeOne : Resource -> Resource
takeOne resource =
    case resource of
        Wood name rarity amount ->
            Wood name rarity (amount - 1)

        Rock name rarity amount ->
            Rock name rarity (amount - 1)

        Flower name rarity amount ->
            Flower name rarity (amount - 1)

        Mushroom name rarity amount ->
            Mushroom name rarity (amount - 1)


woodToString : Wood -> String
woodToString wood =
    case wood of
        Oak ->
            "Oak"

        Elm ->
            "Elm"

        Birch ->
            "Birch"

        Willow ->
            "Willow"


rockToString : Rock -> String
rockToString rock =
    case rock of
        Steel ->
            "Steel"

        Bronze ->
            "Bronze"

        Stone ->
            "Stone"

        Gold ->
            "Gold"


flowerToString : Flower -> String
flowerToString flower =
    case flower of
        Buttercup ->
            "Buttercup"

        Daffodil ->
            "Daffodil"

        Tulip ->
            "Tulip"

        CommonDaisy ->
            "CommonDaisy"

        Rose ->
            "Rose"


mushroomToString : Mushroom -> String
mushroomToString mushroom =
    case mushroom of
        Shiitake ->
            "Shiitake"

        Chanterelle ->
            "Chanterelle"

        Agaricus ->
            "Agaricus"

        Enoki ->
            "Enoki"


getResourceName : Resource -> String
getResourceName resource =
    case resource of
        Wood n _ _ ->
            woodToString n

        Rock n _ _ ->
            rockToString n

        Flower n _ _ ->
            flowerToString n

        Mushroom n _ _ ->
            mushroomToString n


getAmount : Resource -> Int
getAmount resource =
    case resource of
        Wood _ _ a ->
            a

        Rock _ _ a ->
            a

        Flower _ _ a ->
            a

        Mushroom _ _ a ->
            a


getRarity : Resource -> Float
getRarity resource =
    case resource of
        Wood _ r _ ->
            r

        Rock _ r _ ->
            r

        Flower _ r _ ->
            r

        Mushroom _ r _ ->
            r


genAmountChance : Resource -> Generator ( Int, Float )
genAmountChance resource =
    Random.pair
        (Random.int 0 (getAmount resource))
        (Random.float 0 1)
