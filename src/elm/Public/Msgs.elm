module Public.Msgs exposing (..)

import Public.BookReference.Msgs
import Public.ForgottenPassword.Msgs
import Public.ForgottenPasswordConfirmation.Msgs
import Public.Login.Msgs
import Public.Questionnaire.Msgs
import Public.Signup.Msgs
import Public.SignupConfirmation.Msgs


type Msg
    = BookReferenceMsg Public.BookReference.Msgs.Msg
    | ForgottenPasswordMsg Public.ForgottenPassword.Msgs.Msg
    | ForgottenPasswordConfirmationMsg Public.ForgottenPasswordConfirmation.Msgs.Msg
    | LoginMsg Public.Login.Msgs.Msg
    | QuestionnaireMsg Public.Questionnaire.Msgs.Msg
    | SignupMsg Public.Signup.Msgs.Msg
    | SignupConfirmationMsg Public.SignupConfirmation.Msgs.Msg
