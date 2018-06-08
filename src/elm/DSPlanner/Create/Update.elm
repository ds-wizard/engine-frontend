module DSPlanner.Create.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import DSPlanner.Create.Models exposing (Model, QuestionnaireCreateForm, encodeQuestionnaireCreateForm, initQuestionnaireCreateForm, questionnaireCreateFormValidation)
import DSPlanner.Create.Msgs exposing (Msg(..))
import DSPlanner.Requests exposing (postQuestionnaire)
import DSPlanner.Routing exposing (Route(Index))
import Form
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)
import KMPackages.Requests exposing (getPackages)
import Msgs
import Requests exposing (getResultCmd)
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


postQuestionnaireCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postQuestionnaireCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate <| Routing.DSPlanner Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerErrorJwt error "Questionnaire could not be created." }
            , getResultCmd result
            )
