module Msgs exposing (..)

import Auth.Msgs
import Navigation exposing (Location)


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
