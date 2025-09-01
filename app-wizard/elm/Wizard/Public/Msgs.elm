module Wizard.Public.Msgs exposing (Msg(..))

import Wizard.Public.Auth.Msgs
import Wizard.Public.ForgottenPassword.Msgs
import Wizard.Public.ForgottenPasswordConfirmation.Msgs
import Wizard.Public.Login.Msgs
import Wizard.Public.Signup.Msgs
import Wizard.Public.SignupConfirmation.Msgs


type Msg
    = AuthMsg Wizard.Public.Auth.Msgs.Msg
    | ForgottenPasswordMsg Wizard.Public.ForgottenPassword.Msgs.Msg
    | ForgottenPasswordConfirmationMsg Wizard.Public.ForgottenPasswordConfirmation.Msgs.Msg
    | LoginMsg Wizard.Public.Login.Msgs.Msg
    | SignupMsg Wizard.Public.Signup.Msgs.Msg
    | SignupConfirmationMsg Wizard.Public.SignupConfirmation.Msgs.Msg
