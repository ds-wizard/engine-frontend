module Public.Update exposing (fetchData, update)

import Models exposing (State)
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


fetchData : Route -> (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
fetchData route wrapMsg =
    case route of
        BookReference uuid ->
            Public.BookReference.Update.fetchData (wrapMsg << BookReferenceMsg) uuid

        Questionnaire ->
            Public.Questionnaire.Update.fetchData (wrapMsg << QuestionnaireMsg)

        SignupConfirmation userId hash ->
            Public.SignupConfirmation.Update.fetchData (wrapMsg << SignupConfirmationMsg) userId hash

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        BookReferenceMsg brMsg ->
            let
                ( bookReferenceModel, cmd ) =
                    Public.BookReference.Update.update brMsg (wrapMsg << BookReferenceMsg) model.bookReferenceModel
            in
            ( state.seed, { model | bookReferenceModel = bookReferenceModel }, cmd )

        ForgottenPasswordMsg fpMsg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Public.ForgottenPassword.Update.update fpMsg (wrapMsg << ForgottenPasswordMsg) model.forgottenPasswordModel
            in
            ( state.seed, { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg fpcMsg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Public.ForgottenPasswordConfirmation.Update.update fpcMsg (wrapMsg << ForgottenPasswordConfirmationMsg) model.forgottenPasswordConfirmationModel
            in
            ( state.seed, { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

        LoginMsg lMsg ->
            let
                ( loginModel, cmd ) =
                    Public.Login.Update.update lMsg (wrapMsg << LoginMsg) model.loginModel
            in
            ( state.seed, { model | loginModel = loginModel }, cmd )

        QuestionnaireMsg qMsg ->
            let
                ( questionnaireModel, cmd ) =
                    Public.Questionnaire.Update.update qMsg (wrapMsg << QuestionnaireMsg) model.questionnaireModel
            in
            ( state.seed, { model | questionnaireModel = questionnaireModel }, cmd )

        SignupMsg sMsg ->
            let
                ( newSeed, signupModel, cmd ) =
                    Public.Signup.Update.update sMsg (wrapMsg << SignupMsg) state.seed model.signupModel
            in
            ( newSeed, { model | signupModel = signupModel }, cmd )

        SignupConfirmationMsg scMsg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Public.SignupConfirmation.Update.update scMsg model.signupConfirmationModel
            in
            ( state.seed, { model | signupConfirmationModel = signupConfirmationModel }, Cmd.none )
