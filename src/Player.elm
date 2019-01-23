module Player exposing
    ( Player
    , Position
    , Skills
    , Stamina(..)
    , decreaseStamina
    , initialState
    , updateItems
    , updatePosition
    )

import Map exposing (Direction(..), LocationData)
import Maybe exposing (map, withDefault)



-- MODEL, TYPES


type alias Player =
    { position : Position
    , items : List Map.Resource
    , skills : Skills
    , stamina : Stamina
    }


type alias Position =
    { lat : Int, lon : Int }


type alias Skills =
    { strength : Int }


type Stamina
    = Stamina Int


initialState : Player
initialState =
    { position = { lat = 0, lon = 0 }
    , items = []
    , skills = { strength = 0 }
    , stamina = Stamina 100 -- should increase after some time
    }



-- UPDATE


updatePosition : Map.Direction -> Player -> Player
updatePosition direction player =
    let
        upd : Position -> Map.Direction -> Position
        upd position d =
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
    { player | position = upd player.position direction }


updateItems : Player -> LocationData -> Player
updateItems player ( ( resource, _ ), _ ) =
    { player
        | items = resource :: player.items
        , skills = increaseSkill player.skills
    }


increaseSkill : Skills -> Skills
increaseSkill skills =
    { skills | strength = skills.strength + 5 }


decreaseStamina : Player -> Player
decreaseStamina player =
    let
        (Stamina stamina) =
            player.stamina
    in
    { player | stamina = Stamina (stamina - 5) }
