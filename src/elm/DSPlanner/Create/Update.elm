module DSPlanner.Create.Update exposing (fetchData, getPackagesCompleted, handleForm, postQuestionnaireCmd, postQuestionnaireCompleted, setSelectedPackage, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import DSPlanner.Common.Models exposing (Questionnaire)
import DSPlanner.Create.Models exposing (Model, QuestionnaireCreateForm, encodeQuestionnaireCreateForm, initQuestionnaireCreateForm, questionnaireCreateFormValidation)
import DSPlanner.Create.Msgs exposing (Msg(..))
import DSPlanner.Requests exposing (postQuestionnaire)
import DSPlanner.Routing exposing (Route(..))
import Form
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)
import KMPackages.Requests exposing (getPackages)
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getPackages session
        |> Jwt.send GetPackagesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg state.session model

        PostQuestionnaireCompleted result ->
            postQuestionnaireCompleted state model result


getPackagesCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackagesCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = getServerErrorJwt error "Unable to get package list" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


setSelectedPackage : Model -> List PackageDetail -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = initQuestionnaireCreateForm model.selectedPackage }

            else
                model

        _ ->
            model


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


postQuestionnaireCompleted : State -> Model -> Result Jwt.JwtError Questionnaire -> ( Model, Cmd Msgs.Msg )
postQuestionnaireCompleted state model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate state.key <| Routing.DSPlanner <| DSPlanner.Routing.Detail questionnaire.uuid
            )

        Err error ->
            ( { model | savingQuestionnaire = getServerErrorJwt error "Questionnaire could not be created." }
            , getResultCmd result
            )
