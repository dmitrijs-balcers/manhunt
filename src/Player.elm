module Player exposing
    ( Player
    , PlayerPosition
    , PlayerSkills
    , Stamina(..)
    , decreaseStamina
    , updateItems
    , updatePlayerPosition
    )

import Map exposing (Direction(..), LocationData)



-- MODEL, TYPES


type alias Player =
    { position : PlayerPosition
    , items : List Map.Resource
    , skills : PlayerSkills
    , stamina : Stamina
    }


type alias PlayerPosition =
    { lat : Int, lon : Int }


type alias PlayerSkills =
    { strength : Int }


type Stamina
    = Stamina Int



-- UPDATE


updatePlayerPosition : Map.Direction -> Player -> Player
updatePlayerPosition direction player =
    let
        updatePosition : PlayerPosition -> Map.Direction -> PlayerPosition
        updatePosition position d =
            case d of
                North ->
                    { position | lon = position.lon + 1 }

                South ->
                    { position | lon = position.lon - 1 }

                West ->
                    { position | lat = position.lat + 1 }

                East ->
                    { position | lat = position.lat - 1 }
    in
    { player | position = updatePosition player.position direction }


updateItems : Maybe LocationData -> Player -> Player
updateItems maybeLocationData player =
    case maybeLocationData of
        Just ( ( Map.Resource resource, _ ), Map.Amount amount ) ->
            { player
                | items = Map.Resource resource :: player.items
                , skills = increaseSkill player.skills
            }

        Nothing ->
            player


increaseSkill : PlayerSkills -> PlayerSkills
increaseSkill skills =
    { skills | strength = skills.strength + 5 }


decreaseStamina : Player -> Player
decreaseStamina player =
    let
        (Stamina stamina) =
            player.stamina
    in
    { player | stamina = Stamina (stamina - 5) }
