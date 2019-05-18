module Msg exposing (Msg(..))

import Map exposing (Action)
import Time


type Msg
    = Perform Action
    | GenerateWorld Int
    | Tick Time.Posix
    | Keyboard String