module Shared.AbstractAppState exposing (AbstractAppState)

import Shared.Auth.Session exposing (Session)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)


type alias AbstractAppState a =
    { a
        | apiUrl : String
        , session : Session
        , config : BootstrapConfig
    }
