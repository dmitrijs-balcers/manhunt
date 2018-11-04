module Player exposing (Player, PlayerPosition, PlayerSkills, updatePlayerPosition)

import Map exposing (Direction(..))



-- MODEL, TYPES


type alias Player =
    { position : PlayerPosition
    , items : List Map.Resource
    , skills : PlayerSkills
    }


type alias PlayerPosition =
    { lat : Int, lon : Int }


type alias PlayerSkills =
    { strength : Int }



-- UPDATE


updatePlayerPosition : Player -> Map.Direction -> Player
updatePlayerPosition player direction =
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
