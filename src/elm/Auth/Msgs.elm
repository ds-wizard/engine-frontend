module Auth.Msgs exposing (..)

import Http


type Msg
    = Email String
    | Password String
    | Login
    | GetTokenCompleted (Result Http.Error String)
