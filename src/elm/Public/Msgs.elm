module Public.Msgs exposing (..)

import Public.ForgottenPassword.Msgs
import Public.Home.Msgs
import Public.Login.Msgs
import Public.Signup.Msgs


type Msg
    = ForgottenPasswordMsg Public.ForgottenPassword.Msgs.Msg
    | HomeMsg Public.Home.Msgs.Msg
    | LoginMsg Public.Login.Msgs.Msg
    | SignupMsg Public.Signup.Msgs.Msg
