module Wizard.Projects.Create.TemplateCreate.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Form.Field as Field
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Uuid
import Wizard.Common.Api exposing (applyResultCmd, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Msgs
import Wizard.Projects.Common.QuestionnaireFromTemplateCreateForm as QuestionnaireFromTemplateCreateForm
import Wizard.Projects.Create.TemplateCreate.Models exposing (Model)
import Wizard.Projects.Create.TemplateCreate.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case model.selectedQuestionnaire of
        Just uuid ->
            QuestionnairesApi.getQuestionnaire (Uuid.fromUuidString uuid) appState GetTemplateQuestionnaireComplete

        Nothing ->
            Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostQuestionnaireCompleted result ->
            handlePostQuestionnaireCompleted appState model result

        QuestionnaireTypeHintInputMsg typeHintInputMsg ->
            handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model

        GetTemplateQuestionnaireComplete result ->
            applyResultCmd appState
                { setResult = \value record -> { record | templateQuestionnaire = value }
                , defaultError = lg "apiError.questionnaires.getError" appState
                , model = model
                , result = result
                , cmd = Cmd.none
                }


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireFromTemplateCreateForm.encode form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.postQuestionnaireFromTemplate body appState PostQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            ( { model | form = Form.update QuestionnaireFromTemplateCreateForm.validation formMsg model.form }
            , Cmd.none
            )


handlePostQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate appState <| Routes.ProjectsRoute <| DetailRoute questionnaire.uuid ProjectDetailRoute.Questionnaire
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult appState (lg "apiError.questionnaires.postError" appState) error }
            , getResultCmd result
            )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg Questionnaire -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "questionnaireUuid" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << QuestionnaireTypeHintInputMsg
            , getTypeHints = QuestionnairesApi.getQuestionnaires { isTemplate = Just True, userUuids = Nothing, projectTags = Nothing }
            , getError = lg "apiError.packages.getListError" appState
            , setReply = formMsg << Uuid.toString << .uuid
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( questionnaireTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg appState model.questionnaireTypeHintInputModel
    in
    ( { model | questionnaireTypeHintInputModel = questionnaireTypeHintInputModel }, cmd )
