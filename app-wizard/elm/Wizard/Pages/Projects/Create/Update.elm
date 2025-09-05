module Wizard.Pages.Projects.Create.Update exposing (fetchData, update)

import ActionResult
import Cmd.Extra exposing (withNoCmd)
import Common.Api.Request exposing (ToMsg)
import Common.Data.ApiError as ApiError
import Common.Data.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import Common.Utils.Driver as Driver exposing (TourConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Html.Attributes.Extensions exposing (selectDataTour)
import Maybe.Extra as Maybe
import String.Extra as String
import Uuid
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Packages as PackagesApi
import Wizard.Api.Questionnaires as QuestionnaireApi
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.QuestionnaireCreateForm as QuestionnaireCreateForm
import Wizard.Pages.Projects.Create.Models exposing (Model, mapMode, updateDefaultMode)
import Wizard.Pages.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)
import Wizard.Utils.Driver as Driver
import Wizard.Utils.Feature as Feature
import Wizard.Utils.TourId as TourId


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
                    QuestionnaireApi.getQuestionnaireSettings appState templateUuid GetSelectedProjectTemplateCompleted

                _ ->
                    Cmd.none

        fetchSelectedKnowledgeModel =
            case ( createCustom, model.selectedKnowledgeModelId ) of
                ( True, Just kmId ) ->
                    Cmd.batch
                        [ PackagesApi.getPackage appState kmId GetSelectedKnowledgeModelCompleted
                        , KnowledgeModelsApi.fetchPreview appState (Just kmId) [] [] GetKnowledgeModelPreviewCompleted
                        ]

                _ ->
                    Cmd.none

        loadProjectTemplates =
            if createFromTemplate && not anythingPreselected then
                getProjectTemplates appState PaginationQueryString.empty GetProjectTemplatesCountCompleted

            else
                Cmd.none

        loadKnowledgeModels =
            if createCustom && not anythingPreselected then
                PackagesApi.getPackagesSuggestions appState Nothing PaginationQueryString.empty GetKnowledgeModelsCountCompleted

            else
                Cmd.none

        tourCmd =
            if anythingPreselected then
                Cmd.none

            else
                Driver.init (tour appState createFromTemplate createCustom)
    in
    Cmd.batch
        [ fetchSelectedProjectTemplate
        , fetchSelectedKnowledgeModel
        , loadProjectTemplates
        , loadKnowledgeModels
        , tourCmd
        ]


getProjectTemplates : AppState -> PaginationQueryString -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getProjectTemplates appState pqs =
    let
        filters =
            PaginationQueryFilters.create
                [ ( "isTemplate", Just (Bool.toString True) )
                , ( "isMigrating", Just (Bool.toString False) )
                ]
                []
    in
    QuestionnaireApi.getQuestionnaires appState filters pqs


tour : AppState -> Bool -> Bool -> TourConfig
tour appState createFromTemplate createCustom =
    let
        createStep =
            if createFromTemplate && createCustom then
                { element = Just ".nav-underline-tabs"
                , popover =
                    { title = gettext "Starting Point" appState.locale
                    , description = gettext "Create a project from a template with preset content, or start from a knowledge model and configure everything yourself." appState.locale
                    }
                }

            else if createFromTemplate then
                { element = selectDataTour "form-group_templateId"
                , popover =
                    { title = gettext "Project Template" appState.locale
                    , description = gettext "Project templates are pre-configured starting points for your project." appState.locale
                    }
                }

            else
                { element = selectDataTour "form-group_packageId"
                , popover =
                    { title = gettext "Knowledge Model" appState.locale
                    , description = gettext "A knowledge model defines the structure of your questionnaire. You can configure the document template and other settings later." appState.locale
                    }
                }
    in
    Driver.fromAppState TourId.projectsCreate appState
        |> Driver.addStep
            { element = selectDataTour "form-group_name"
            , popover =
                { title = gettext "Project Name" appState.locale
                , description = gettext "Choose a name for your project. You can change it later." appState.locale
                }
            }
        |> Driver.addStep createStep


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        updateModelDefaultMode ( m, c ) =
            ( updateDefaultMode appState m, c )
    in
    case msg of
        GetSelectedProjectTemplateCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value record -> { record | selectedProjectTemplate = ActionResult.map .data value }
                , defaultError = gettext "Unable to get selected project template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        GetSelectedKnowledgeModelCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value record -> { record | selectedKnowledgeModel = value }
                , defaultError = gettext "Unable to get selected knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        GetProjectTemplatesCountCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = \value record -> { record | anyProjectTemplates = value }
                , defaultError = gettext "Unable to get project templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                , locale = appState.locale
                }
                |> updateModelDefaultMode

        GetKnowledgeModelsCountCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = \value record -> { record | anyKnowledgeModels = value }
                , defaultError = gettext "Unable to get knowledge models." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                , locale = appState.locale
                }
                |> updateModelDefaultMode

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl (Routes.projectsIndex appState)) )

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
                    , Cmd.map wrapMsg <| request appState body PostQuestionnaireCompleted
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
                                    KnowledgeModelsApi.fetchPreview appState (Just packageId) [] [] GetKnowledgeModelPreviewCompleted
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
                    , cmdNavigate appState <| Routes.projectsDetail questionnaire.uuid
                    )

                Err error ->
                    ( { model | savingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
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
                    RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
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
                    , getTypeHints = getProjectTemplates appState
                    , getError = gettext "Unable to get project templates." appState.locale
                    , setReply = formMsg << Uuid.toString << .uuid
                    , clearReply = Just <| formMsg ""
                    , filterResults = Nothing
                    }

                ( projectTemplateTypeHintInputModel, cmd ) =
                    TypeHintInput.update cfg typeHintInputMsg model.projectTemplateTypeHintInputModel
            in
            ( { model | projectTemplateTypeHintInputModel = projectTemplateTypeHintInputModel }, cmd )

        KnowledgeModelTypeHintInputMsg typeHintInputMsg ->
            let
                formMsg =
                    wrapMsg << FormMsg << Form.Input "packageId" Form.Select << Field.String

                cfg =
                    { wrapMsg = wrapMsg << KnowledgeModelTypeHintInputMsg
                    , getTypeHints = PackagesApi.getPackagesSuggestions appState Nothing
                    , getError = gettext "Unable to get knowledge models." appState.locale
                    , setReply = formMsg << .id
                    , clearReply = Just <| formMsg ""
                    , filterResults = Nothing
                    }

                ( knowledgeModelTypeHintInputModel, cmd ) =
                    TypeHintInput.update cfg typeHintInputMsg model.knowledgeModelTypeHintInputModel
            in
            ( { model | knowledgeModelTypeHintInputModel = knowledgeModelTypeHintInputModel }, cmd )
