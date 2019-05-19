module Msg exposing (Msg(..))

import Craft exposing (AlchemyResource, Craft, SmithResource)
import Map exposing (Action)
import Time


type Msg
    = Perform Action
    | GenerateWorld Int
    | Tick Time.Posix
    | Keyboard String
    | SelectCraft Craft
    | AddToAlchemyCraft AlchemyResource
    | AddToSmithingCraft SmithResource
