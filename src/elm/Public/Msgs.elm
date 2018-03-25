module Public.Msgs exposing (..)

import Public.ForgottenPassword.Msgs
import Public.ForgottenPasswordConfirmation.Msgs
import Public.Home.Msgs
import Public.Login.Msgs
import Public.Signup.Msgs
import Public.SignupConfirmation.Msgs


type Msg
    = ForgottenPasswordMsg Public.ForgottenPassword.Msgs.Msg
    | ForgottenPasswordConfirmationMsg Public.ForgottenPasswordConfirmation.Msgs.Msg
    | HomeMsg Public.Home.Msgs.Msg
    | LoginMsg Public.Login.Msgs.Msg
    | SignupMsg Public.Signup.Msgs.Msg
    | SignupConfirmationMsg Public.SignupConfirmation.Msgs.Msg
