module Player exposing
    ( Player
    , Position
    , Skills
    , Stamina(..)
    , decreaseStamina
    , increaseStamina
    , initialState
    , updateItems
    , updatePosition
    )

import Dict exposing (Dict)
import Map exposing (Direction(..), LocationData)
import Resource exposing (Resource)



-- MODEL, TYPES


type alias Player =
    { position : Position
    , items : Dict String ( Resource, Int )
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
    , items = Dict.empty
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


updateItems : Player -> Resource -> Player
updateItems player resource =
    let
        update : Maybe ( Resource, Int ) -> Maybe ( Resource, Int )
        update c =
            case c of
                Just ( r, a ) ->
                    Just ( r, a + 1 )

                Nothing ->
                    Just ( resource, 1 )
    in
    { player
        | items = Dict.update (Resource.getResourceName resource) update player.items
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


increaseStamina : Player -> Player
increaseStamina player =
    let
        (Stamina stamina) =
            player.stamina
    in
    { player | stamina = Stamina (stamina + 1) }
