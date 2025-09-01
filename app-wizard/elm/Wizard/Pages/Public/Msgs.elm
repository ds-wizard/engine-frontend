module Wizard.Pages.Public.Msgs exposing (Msg(..))

import Wizard.Pages.Public.Auth.Msgs
import Wizard.Pages.Public.ForgottenPassword.Msgs
import Wizard.Pages.Public.ForgottenPasswordConfirmation.Msgs
import Wizard.Pages.Public.Login.Msgs
import Wizard.Pages.Public.Signup.Msgs
import Wizard.Pages.Public.SignupConfirmation.Msgs


type Msg
    = AuthMsg Wizard.Pages.Public.Auth.Msgs.Msg
    | ForgottenPasswordMsg Wizard.Pages.Public.ForgottenPassword.Msgs.Msg
    | ForgottenPasswordConfirmationMsg Wizard.Pages.Public.ForgottenPasswordConfirmation.Msgs.Msg
    | LoginMsg Wizard.Pages.Public.Login.Msgs.Msg
    | SignupMsg Wizard.Pages.Public.Signup.Msgs.Msg
    | SignupConfirmationMsg Wizard.Pages.Public.SignupConfirmation.Msgs.Msg
