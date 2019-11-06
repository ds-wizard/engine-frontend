module Wizard.Public.Login.Msgs exposing (Msg(..))

import ActionResult exposing (ActionResult)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = Email String
    | Password String
    | DoLogin
    | LoginCompleted (Result ApiError String)
    | GetProfileInfoFailed (ActionResult String)
