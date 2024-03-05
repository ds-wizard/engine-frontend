module Wizard.Projects.Create.Update exposing (fetchData, update)

import ActionResult
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Shared.Api exposing (ToMsg)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError
import Shared.Utils exposing (boolToString, withNoCmd)
import String.Extra as String
import Uuid
import Wizard.Common.Api exposing (applyResult, applyResultTransform, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Feature as Feature
import Wizard.Msgs
import Wizard.Projects.Common.QuestionnaireCreateForm as QuestionnaireCreateForm
import Wizard.Projects.Create.Models exposing (Model, mapMode, updateDefaultMode)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        createFromTemplate =
            Feature.projectsCreateFromTemplate appState

        createCustom =
            Feature.projectsCreateCustom appState

        anythingPreselected =
            Maybe.isJust model.selectedProjectTemplateUuid || Maybe.isJust model.selectedKnowledgeModelId

        fetchSelectedProjectTemplate =
            case ( createFromTemplate, model.selectedProjectTemplateUuid ) of
                ( True, Just templateUuid ) ->
                    QuestionnaireApi.getQuestionnaire templateUuid appState GetSelectedProjectTemplateCompleted

                _ ->
                    Cmd.none

        fetchSelectedKnowledgeModel =
            case ( createCustom, model.selectedKnowledgeModelId ) of
                ( True, Just kmId ) ->
                    Cmd.batch
                        [ PackagesApi.getPackage kmId appState GetSelectedKnowledgeModelCompleted
                        , KnowledgeModelsApi.fetchPreview (Just kmId) [] [] appState GetKnowledgeModelPreviewCompleted
                        ]

                _ ->
                    Cmd.none

        loadProjectTemplates =
            if createFromTemplate && not anythingPreselected then
                getProjectTemplates PaginationQueryString.empty appState GetProjectTemplatesCountCompleted

            else
                Cmd.none

        loadKnowledgeModels =
            if createCustom && not anythingPreselected then
                PackagesApi.getPackagesSuggestions Nothing PaginationQueryString.empty appState GetKnowledgeModelsCountCompleted

            else
                Cmd.none
    in
    Cmd.batch
        [ fetchSelectedProjectTemplate
        , fetchSelectedKnowledgeModel
        , loadProjectTemplates
        , loadKnowledgeModels
        ]


getProjectTemplates : PaginationQueryString -> AppState -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getProjectTemplates =
    let
        filters =
            PaginationQueryFilters.create
                [ ( "isTemplate", Just (boolToString True) )
                , ( "isMigrating", Just (boolToString False) )
                ]
                []
    in
    QuestionnaireApi.getQuestionnaires filters


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        updateModelDefaultMode ( m, c ) =
            ( updateDefaultMode appState m, c )
    in
    case msg of
        GetSelectedProjectTemplateCompleted result ->
            applyResult appState
                { setResult = \value record -> { record | selectedProjectTemplate = value }
                , defaultError = gettext "Unable to get selected project template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        GetSelectedKnowledgeModelCompleted result ->
            applyResult appState
                { setResult = \value record -> { record | selectedKnowledgeModel = value }
                , defaultError = gettext "Unable to get selected knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        GetProjectTemplatesCountCompleted result ->
            applyResultTransform appState
                { setResult = \value record -> { record | anyProjectTemplates = value }
                , defaultError = gettext "Unable to get project templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                }
                |> updateModelDefaultMode

        GetKnowledgeModelsCountCompleted result ->
            applyResultTransform appState
                { setResult = \value record -> { record | anyKnowledgeModels = value }
                , defaultError = gettext "Unable to get knowledge models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                }
                |> updateModelDefaultMode

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        projectTemplateModeRequest =
                            ( QuestionnaireCreateForm.encodeFromTemplate form
                            , QuestionnaireApi.postQuestionnaireFromTemplate
                            )

                        knowledgeModelModeRequest =
                            let
                                selectedTags =
                                    if model.useAllQuestions then
                                        []

                                    else
                                        model.selectedTags
                            in
                            ( QuestionnaireCreateForm.encodeFromPackage selectedTags form
                            , QuestionnaireApi.postQuestionnaire
                            )

                        ( body, request ) =
                            mapMode model projectTemplateModeRequest knowledgeModelModeRequest
                    in
                    ( { model | savingQuestionnaire = ActionResult.Loading }
                    , Cmd.map wrapMsg <| request body appState PostQuestionnaireCompleted
                    )

                _ ->
                    let
                        validationMode =
                            mapMode model
                                QuestionnaireCreateForm.TemplateValidationMode
                                QuestionnaireCreateForm.PackageValidationMode

                        newModel =
                            { model | form = Form.update (QuestionnaireCreateForm.validation validationMode) formMsg model.form }

                        selectedPackage =
                            Maybe.andThen String.toMaybe (Form.getFieldAsString "packageId" newModel.form).value
                    in
                    case selectedPackage of
                        Just packageId ->
                            if newModel.lastFetchedPreview /= Just packageId then
                                ( { newModel
                                    | lastFetchedPreview = Just packageId
                                    , knowledgeModelPreview = ActionResult.Loading
                                    , selectedTags = []
                                  }
                                , Cmd.map wrapMsg <|
                                    KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted
                                )

                            else
                                ( newModel, Cmd.none )

                        Nothing ->
                            ( { newModel
                                | lastFetchedPreview = Nothing
                                , knowledgeModelPreview = ActionResult.Unset
                                , selectedTags = []
                              }
                            , Cmd.none
                            )

        PostQuestionnaireCompleted result ->
            case result of
                Ok questionnaire ->
                    ( model
                    , cmdNavigate appState <| Routes.projectsDetailQuestionnaire questionnaire.uuid Nothing
                    )

                Err error ->
                    ( { model | savingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )

        AddTag tagUuid ->
            withNoCmd { model | selectedTags = tagUuid :: model.selectedTags }

        RemoveTag tagUuid ->
            withNoCmd { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }

        ChangeUseAllQuestions value ->
            ( { model | useAllQuestions = value }, Cmd.none )

        GetKnowledgeModelPreviewCompleted result ->
            let
                newModel =
                    case result of
                        Ok knowledgeModel ->
                            { model | knowledgeModelPreview = ActionResult.Success knowledgeModel }

                        Err error ->
                            { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the knowledge model." appState.locale) error }

                cmd =
                    getResultCmd Wizard.Msgs.logoutMsg result
            in
            ( newModel, cmd )

        SetActiveTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        ProjectTemplateTypeHintInputMsg typeHintInputMsg ->
            let
                formMsg =
                    wrapMsg << FormMsg << Form.Input "templateId" Form.Select << Field.String

                cfg =
                    { wrapMsg = wrapMsg << ProjectTemplateTypeHintInputMsg
                    , getTypeHints = getProjectTemplates
                    , getError = gettext "Unable to get project templates." appState.locale
                    , setReply = formMsg << Uuid.toString << .uuid
                    , clearReply = Just <| formMsg ""
                    , filterResults = Nothing
                    }

                ( projectTemplateTypeHintInputModel, cmd ) =
                    TypeHintInput.update cfg typeHintInputMsg appState model.projectTemplateTypeHintInputModel
            in
            ( { model | projectTemplateTypeHintInputModel = projectTemplateTypeHintInputModel }, cmd )

        KnowledgeModelTypeHintInputMsg typeHintInputMsg ->
            let
                formMsg =
                    wrapMsg << FormMsg << Form.Input "packageId" Form.Select << Field.String

                cfg =
                    { wrapMsg = wrapMsg << KnowledgeModelTypeHintInputMsg
                    , getTypeHints = PackagesApi.getPackagesSuggestions Nothing
                    , getError = gettext "Unable to get knowledge models." appState.locale
                    , setReply = formMsg << .id
                    , clearReply = Just <| formMsg ""
                    , filterResults = Nothing
                    }

                ( knowledgeModelTypeHintInputModel, cmd ) =
                    TypeHintInput.update cfg typeHintInputMsg appState model.knowledgeModelTypeHintInputModel
            in
            ( { model | knowledgeModelTypeHintInputModel = knowledgeModelTypeHintInputModel }, cmd )
