module Public.Update exposing (fetchData, update)

import Common.AppState exposing (AppState)
import Msgs
import Public.BookReference.Update
import Public.ForgottenPassword.Update
import Public.ForgottenPasswordConfirmation.Update
import Public.Login.Update
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))
import Public.Questionnaire.Update
import Public.Routes exposing (Route(..))
import Public.Signup.Update
import Public.SignupConfirmation.Update


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        BookReferenceRoute uuid ->
            Cmd.map BookReferenceMsg <|
                Public.BookReference.Update.fetchData uuid appState

        QuestionnaireRoute ->
            Cmd.map QuestionnaireMsg <|
                Public.Questionnaire.Update.fetchData appState

        SignupConfirmationRoute userId hash ->
            Cmd.map SignupConfirmationMsg <|
                Public.SignupConfirmation.Update.fetchData userId hash appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        BookReferenceMsg brMsg ->
            let
                ( bookReferenceModel, cmd ) =
                    Public.BookReference.Update.update brMsg appState model.bookReferenceModel
            in
            ( { model | bookReferenceModel = bookReferenceModel }, cmd )

        ForgottenPasswordMsg fpMsg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Public.ForgottenPassword.Update.update fpMsg (wrapMsg << ForgottenPasswordMsg) appState model.forgottenPasswordModel
            in
            ( { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg fpcMsg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Public.ForgottenPasswordConfirmation.Update.update fpcMsg (wrapMsg << ForgottenPasswordConfirmationMsg) appState model.forgottenPasswordConfirmationModel
            in
            ( { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

        LoginMsg lMsg ->
            let
                ( loginModel, cmd ) =
                    Public.Login.Update.update lMsg (wrapMsg << LoginMsg) appState model.loginModel
            in
            ( { model | loginModel = loginModel }, cmd )

        QuestionnaireMsg qMsg ->
            let
                ( questionnaireModel, cmd ) =
                    Public.Questionnaire.Update.update qMsg (wrapMsg << QuestionnaireMsg) appState model.questionnaireModel
            in
            ( { model | questionnaireModel = questionnaireModel }, cmd )

        SignupMsg sMsg ->
            let
                ( signupModel, cmd ) =
                    Public.Signup.Update.update sMsg (wrapMsg << SignupMsg) appState model.signupModel
            in
            ( { model | signupModel = signupModel }, cmd )

        SignupConfirmationMsg scMsg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Public.SignupConfirmation.Update.update scMsg appState model.signupConfirmationModel
            in
            ( { model | signupConfirmationModel = signupConfirmationModel }, cmd )
