module Public.Login.Msgs exposing (..)

import ActionResult exposing (ActionResult)
import Http


type Msg
    = Email String
    | Password String
    | Login
    | LoginCompleted (Result Http.Error String)
    | GetProfileInfoFailed (ActionResult String)
