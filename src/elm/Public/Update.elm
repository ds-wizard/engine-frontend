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
import Public.Routing exposing (Route(..))
import Public.Signup.Update
import Public.SignupConfirmation.Update
import Random exposing (Seed)


fetchData : Route -> (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData route wrapMsg appState =
    case route of
        BookReference uuid ->
            Public.BookReference.Update.fetchData (wrapMsg << BookReferenceMsg) uuid appState

        Questionnaire ->
            Public.Questionnaire.Update.fetchData (wrapMsg << QuestionnaireMsg) appState

        SignupConfirmation userId hash ->
            Public.SignupConfirmation.Update.fetchData (wrapMsg << SignupConfirmationMsg) userId hash appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        BookReferenceMsg brMsg ->
            let
                ( bookReferenceModel, cmd ) =
                    Public.BookReference.Update.update brMsg (wrapMsg << BookReferenceMsg) model.bookReferenceModel
            in
            ( appState.seed, { model | bookReferenceModel = bookReferenceModel }, cmd )

        ForgottenPasswordMsg fpMsg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Public.ForgottenPassword.Update.update fpMsg (wrapMsg << ForgottenPasswordMsg) appState model.forgottenPasswordModel
            in
            ( appState.seed, { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg fpcMsg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Public.ForgottenPasswordConfirmation.Update.update fpcMsg (wrapMsg << ForgottenPasswordConfirmationMsg) appState model.forgottenPasswordConfirmationModel
            in
            ( appState.seed, { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

        LoginMsg lMsg ->
            let
                ( loginModel, cmd ) =
                    Public.Login.Update.update lMsg (wrapMsg << LoginMsg) appState model.loginModel
            in
            ( appState.seed, { model | loginModel = loginModel }, cmd )

        QuestionnaireMsg qMsg ->
            let
                ( questionnaireModel, cmd ) =
                    Public.Questionnaire.Update.update qMsg (wrapMsg << QuestionnaireMsg) appState model.questionnaireModel
            in
            ( appState.seed, { model | questionnaireModel = questionnaireModel }, cmd )

        SignupMsg sMsg ->
            let
                ( newSeed, signupModel, cmd ) =
                    Public.Signup.Update.update sMsg (wrapMsg << SignupMsg) appState model.signupModel
            in
            ( newSeed, { model | signupModel = signupModel }, cmd )

        SignupConfirmationMsg scMsg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Public.SignupConfirmation.Update.update scMsg model.signupConfirmationModel
            in
            ( appState.seed, { model | signupConfirmationModel = signupConfirmationModel }, Cmd.none )
