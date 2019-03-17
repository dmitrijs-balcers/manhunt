module Msg exposing (Msg(..))

import Map exposing (Action)
import Model exposing (KeyboardAction)
import Time


type Msg
    = Perform Action
    | GenerateWorld Int
    | Tick Time.Posix
    | Keyboard String
