module Public.Login.Msgs exposing (..)

import Http


type Msg
    = Email String
    | Password String
    | Login
    | LoginCompleted (Result Http.Error String)
    | GetProfileInfoFailed
