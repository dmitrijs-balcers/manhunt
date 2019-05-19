module Msg exposing (Craft(..), Msg(..))

import Map exposing (Action)
import Time


type Msg
    = Perform Action
    | GenerateWorld Int
    | Tick Time.Posix
    | Keyboard String
    | SelectCraft Craft


type Craft
    = Alchemy
    | Smith
