module Shared.AbstractAppState exposing (AbstractAppState)

import Shared.Auth.Session exposing (Session)


type alias AbstractAppState a =
    { a
        | apiUrl : String
        , session : Session
    }
