module Public.View exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html)
import Public.BookReference.View
import Public.ForgottenPassword.View
import Public.ForgottenPasswordConfirmation.View
import Public.Login.View
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))
import Public.Questionnaire.View
import Public.Routes exposing (Route(..))
import Public.Signup.View
import Public.SignupConfirmation.View


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        BookReferenceRoute _ ->
            Html.map BookReferenceMsg <|
                Public.BookReference.View.view appState model.bookReferenceModel

        ForgottenPasswordRoute ->
            Html.map ForgottenPasswordMsg <|
                Public.ForgottenPassword.View.view appState model.forgottenPasswordModel

        ForgottenPasswordConfirmationRoute _ _ ->
            Html.map ForgottenPasswordConfirmationMsg <|
                Public.ForgottenPasswordConfirmation.View.view appState model.forgottenPasswordConfirmationModel

        LoginRoute ->
            Html.map LoginMsg <|
                Public.Login.View.view appState model.loginModel

        QuestionnaireRoute ->
            Html.map QuestionnaireMsg <|
                Public.Questionnaire.View.view appState model.questionnaireModel

        SignupRoute ->
            Html.map SignupMsg <|
                Public.Signup.View.view appState model.signupModel

        SignupConfirmationRoute _ _ ->
            Html.map SignupConfirmationMsg <|
                Public.SignupConfirmation.View.view appState model.signupConfirmationModel
