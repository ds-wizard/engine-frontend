module Msgs exposing (..)

import Navigation exposing (Location)


type Msg
    = ChangeLocation String
    | OnLocationChange Location
