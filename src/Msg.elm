module Msg exposing (Craft(..), Msg(..))

import Map exposing (Action)
import Resource exposing (AlchemyResource, Rock, SmithResource)
import Time


type Msg
    = Perform Action
    | GenerateWorld Int
    | Tick Time.Posix
    | Keyboard String
    | SelectCraft Craft
    | AddToAlchemyCraft AlchemyResource
    | AddToSmithingCraft SmithResource


type Craft
    = Alchemy
    | Smith
