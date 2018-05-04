module Questionnaires.Create.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Form
import Jwt
import Msgs
import PackageManagement.Models exposing (PackageDetail)
import PackageManagement.Requests exposing (getPackages)
import Questionnaires.Create.Models exposing (Model, QuestionnaireCreateForm, encodeQuestionnaireCreateForm, questionnaireCreateFormValidation)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (postQuestionnaire)
import Questionnaires.Routing exposing (Route(Index))
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getPackages session
        |> Jwt.send GetPackagesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result

        FormMsg msg ->
            handleForm msg wrapMsg session model

        PostQuestionnaireCompleted result ->
            postQuestionnaireCompleted model result


getPackagesCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackagesCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success packages }

                Err error ->
                    { model | packages = Error "Unable to get package list" }
    in
    ( newModel, Cmd.none )


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                cmd =
                    postQuestionnaireCmd wrapMsg session form
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update questionnaireCreateFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


postQuestionnaireCmd : (Msg -> Msgs.Msg) -> Session -> QuestionnaireCreateForm -> Cmd Msgs.Msg
postQuestionnaireCmd wrapMsg session form =
    form
        |> encodeQuestionnaireCreateForm
        |> postQuestionnaire session
        |> Jwt.send PostQuestionnaireCompleted
        |> Cmd.map wrapMsg


postQuestionnaireCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postQuestionnaireCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate <| Routing.Questionnaires Index )

        Err error ->
            ( { model | savingQuestionnaire = Error "Questionnaire could not be created." }, Cmd.none )
