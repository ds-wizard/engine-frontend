module Public.Login.Msgs exposing (Msg(..))

import ActionResult exposing (ActionResult)
import Http


type Msg
    = Email String
    | Password String
    | DoLogin
    | LoginCompleted (Result Http.Error String)
    | GetProfileInfoFailed (ActionResult String)
