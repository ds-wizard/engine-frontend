module Common.Pages.Projects.Create exposing
    ( ActiveTab
    , Mode
    , Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Create a project, from a project template or from a knowledge model.

Where a project may be created from both and both exist, the starting point is
a choice (a radio group, each option saying what it means); where only one is
allowed or only one kind exists, that one is used and there is no choice to
make. A template or a knowledge model given in the URL
(`?selectedProjectTemplate`, `?selectedKnowledgeModel`) is shown, not chosen.

From a knowledge model, its language can be picked when it has translations, and
its questions can be narrowed down by question tags. The tags come from a preview
of the knowledge model, fetched whenever a different one is chosen.

On success the project opens.

-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Common.Api.Projects as ProjectsApi
import Common.Api.Request exposing (ToMsg)
import Common.Components.Form as Form
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.Tour as Tour
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.Forms.ProjectCreateForm as ProjectCreateForm exposing (ProjectCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.Session as Session
import Common.Data.TourId as TourId
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Bool as Bool
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Form as ElmForm exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Field as Field
import Ui.Layout as Layout
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { selectedProjectTemplateUuid : Maybe Uuid
    , selectedKnowledgeModelUuid : Maybe Uuid
    , selectedProjectTemplate : ActionResult ProjectSettings
    , selectedKnowledgeModel : ActionResult KnowledgeModelPackageDetail
    , projectTemplateTypeHint : TypeHint.Model Project
    , knowledgeModelTypeHint : TypeHint.Model KnowledgeModelPackageSuggestion
    , anyProjectTemplates : ActionResult Bool
    , anyKnowledgeModels : ActionResult Bool
    , form : Form FormError ProjectCreateForm
    , mode : Mode
    , savingProject : ActionResult ()
    , selectedTags : List String
    , useAllQuestions : Bool
    , lastFetchedPreview : Maybe String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , activeTab : ActiveTab
    }


type Mode
    = FromProjectTemplateMode
    | FromKnowledgeModelMode
    | DefaultMode DefaultMode


type DefaultMode
    = TabsDefaultMode
    | ProjectTemplateDefaultMode
    | KnowledgeModelDefaultMode


type ActiveTab
    = ProjectTemplateTab
    | KnowledgeModelTab


initialModel : AppState -> Maybe Uuid -> Maybe Uuid -> Model
initialModel appState selectedProjectTemplateUuid selectedKnowledgeModelUuid =
    let
        mode =
            case ( selectedProjectTemplateUuid, selectedKnowledgeModelUuid ) of
                ( Just _, _ ) ->
                    FromProjectTemplateMode

                ( _, Just _ ) ->
                    FromKnowledgeModelMode

                _ ->
                    DefaultMode TabsDefaultMode

        loadingIfJust mbUuid =
            if Maybe.isJust mbUuid then
                ActionResult.Loading

            else
                ActionResult.Unset

        anythingPreselected =
            Maybe.isJust selectedProjectTemplateUuid || Maybe.isJust selectedKnowledgeModelUuid

        loadingIf condition =
            if condition then
                ActionResult.Loading

            else
                ActionResult.Unset
    in
    { selectedProjectTemplateUuid = selectedProjectTemplateUuid
    , selectedKnowledgeModelUuid = selectedKnowledgeModelUuid
    , selectedProjectTemplate = loadingIfJust selectedProjectTemplateUuid
    , selectedKnowledgeModel = loadingIfJust selectedKnowledgeModelUuid
    , projectTemplateTypeHint = TypeHint.init
    , knowledgeModelTypeHint = TypeHint.init
    , anyProjectTemplates = loadingIf (Feature.projectsCreateFromTemplate appState && not anythingPreselected)
    , anyKnowledgeModels = loadingIf (Feature.projectsCreateCustom appState && not anythingPreselected)
    , form = ProjectCreateForm.init appState ProjectCreateForm.TemplateValidationMode selectedProjectTemplateUuid selectedKnowledgeModelUuid
    , mode = mode
    , savingProject = ActionResult.Unset
    , selectedTags = []
    , useAllQuestions = True
    , lastFetchedPreview = Maybe.map Uuid.toString selectedKnowledgeModelUuid
    , knowledgeModelPreview = loadingIfJust selectedKnowledgeModelUuid
    , activeTab = ProjectTemplateTab
    }


mapMode : Model -> a -> a -> a
mapMode model fromProjectTemplateMode fromKnowledgeModelMode =
    case model.mode of
        FromProjectTemplateMode ->
            fromProjectTemplateMode

        FromKnowledgeModelMode ->
            fromKnowledgeModelMode

        DefaultMode ProjectTemplateDefaultMode ->
            fromProjectTemplateMode

        DefaultMode KnowledgeModelDefaultMode ->
            fromKnowledgeModelMode

        DefaultMode TabsDefaultMode ->
            case model.activeTab of
                ProjectTemplateTab ->
                    fromProjectTemplateMode

                KnowledgeModelTab ->
                    fromKnowledgeModelMode


updateDefaultMode : AppState -> Model -> Model
updateDefaultMode appState model =
    let
        createFromProjectTemplates =
            Feature.projectsCreateFromTemplate appState

        createFromKnowledgeModels =
            Feature.projectsCreateCustom appState
    in
    if createFromProjectTemplates && not createFromKnowledgeModels then
        { model | mode = DefaultMode ProjectTemplateDefaultMode }

    else if not createFromProjectTemplates && createFromKnowledgeModels then
        { model | mode = DefaultMode KnowledgeModelDefaultMode }

    else
        case ( model.anyKnowledgeModels, model.anyProjectTemplates ) of
            ( ActionResult.Success True, ActionResult.Success False ) ->
                { model | mode = DefaultMode KnowledgeModelDefaultMode }

            ( ActionResult.Success False, ActionResult.Success True ) ->
                { model | mode = DefaultMode ProjectTemplateDefaultMode }

            _ ->
                { model | mode = DefaultMode TabsDefaultMode }


init : AppState -> Maybe Uuid -> Maybe Uuid -> ( Model, Cmd Msg )
init appState selectedProjectTemplateUuid selectedKnowledgeModelUuid =
    let
        model =
            initialModel appState selectedProjectTemplateUuid selectedKnowledgeModelUuid

        createFromTemplate =
            Feature.projectsCreateFromTemplate appState

        createCustom =
            Feature.projectsCreateCustom appState

        anythingPreselected =
            Maybe.isJust selectedProjectTemplateUuid || Maybe.isJust selectedKnowledgeModelUuid

        fetchSelectedProjectTemplate =
            case ( createFromTemplate, selectedProjectTemplateUuid ) of
                ( True, Just templateUuid ) ->
                    ProjectsApi.getSettings appState templateUuid GetSelectedProjectTemplateCompleted

                _ ->
                    Cmd.none

        fetchSelectedKnowledgeModel =
            case ( createCustom, selectedKnowledgeModelUuid ) of
                ( True, Just kmUuid ) ->
                    Cmd.batch
                        [ KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmUuid GetSelectedKnowledgeModelCompleted
                        , KnowledgeModelsApi.fetchPreview appState (Just kmUuid) [] [] GetKnowledgeModelPreviewCompleted
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
                KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing PaginationQueryString.empty GetKnowledgeModelsCountCompleted

            else
                Cmd.none

        -- With counts to wait for, the tour starts when they arrive: they
        -- decide what the form asks first (`withDefaultMode` in `update`).
        tourCmd =
            if anythingPreselected || countsPending model then
                Cmd.none

            else
                tour appState model
    in
    ( model
    , Cmd.batch
        [ fetchSelectedProjectTemplate
        , fetchSelectedKnowledgeModel
        , loadProjectTemplates
        , loadKnowledgeModels
        , tourCmd
        ]
    )


getProjectTemplates : AppState -> PaginationQueryString -> ToMsg (Pagination Project) msg -> Cmd msg
getProjectTemplates appState pqs =
    let
        filters =
            PaginationQueryFilters.create
                [ ( "isTemplate", Just (Bool.toString True) ) ]
                []
    in
    ProjectsApi.getList appState filters pqs


projectTemplatesTypeHintsQueryString : PaginationQueryString -> PaginationQueryString
projectTemplatesTypeHintsQueryString pqs =
    pqs
        |> PaginationQueryString.withSort (Just "name") PaginationQueryString.SortASC
        |> PaginationQueryString.withSize (Just 50)


{-| Whether what the form starts from is still being decided: whether there are
any project templates and knowledge models to start from at all.
-}
countsPending : Model -> Bool
countsPending model =
    ActionResult.isLoading model.anyProjectTemplates || ActionResult.isLoading model.anyKnowledgeModels


{-| The tour of the form as it is drawn. The step after the name is what the
form then asks: the choice of a starting point only where there is a choice,
which is not where a workspace has no project templates.
-}
tour : AppState -> Model -> Cmd msg
tour appState model =
    let
        projectTemplateStep =
            { element = Tour.byField projectTemplateId
            , title = gettext "Project Template" appState.locale
            , description = gettext "Project templates are pre-configured starting points for your project." appState.locale
            }

        knowledgeModelStep =
            { element = Tour.byField knowledgeModelId
            , title = gettext "Knowledge Model" appState.locale
            , description = gettext "A knowledge model defines the structure of your questionnaire. You can configure the document template and other settings later." appState.locale
            }

        createStep =
            case model.mode of
                DefaultMode TabsDefaultMode ->
                    { element = Tour.byId startingPointId
                    , title = gettext "Starting Point" appState.locale
                    , description = gettext "Create a project from a template with preset content, or start from a knowledge model and configure everything yourself." appState.locale
                    }

                DefaultMode ProjectTemplateDefaultMode ->
                    projectTemplateStep

                FromProjectTemplateMode ->
                    projectTemplateStep

                DefaultMode KnowledgeModelDefaultMode ->
                    knowledgeModelStep

                FromKnowledgeModelMode ->
                    knowledgeModelStep
    in
    Tour.tour appState
        { tourId = TourId.projectsCreate
        , completedTours = appState.config.tours
        , enabled = appState.config.features.toursEnabled && Session.exists appState.session
        , steps =
            [ { element = Tour.byField (FormField.fieldId "name")
              , title = gettext "Project Name" appState.locale
              , description = gettext "Choose a name for your project. You can change it later." appState.locale
              }
            , createStep
            ]
        }


startingPointId : String
startingPointId =
    "project-create-starting-point"


projectTemplateId : String
projectTemplateId =
    FormField.fieldId "projectUuid"


knowledgeModelId : String
knowledgeModelId =
    FormField.fieldId "knowledgeModelPackageUuid"



-- UPDATE


type Msg
    = GetSelectedProjectTemplateCompleted (Result ApiError (ProjectDetailWrapper ProjectSettings))
    | GetSelectedKnowledgeModelCompleted (Result ApiError KnowledgeModelPackageDetail)
    | GetProjectTemplatesCountCompleted (Result ApiError (Pagination Project))
    | GetKnowledgeModelsCountCompleted (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | Cancel
    | FormMsg ElmForm.Msg
    | PostProjectCompleted (Result ApiError Project)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | ToggleTag String
    | ChangeUseAllQuestions Bool
    | SetActiveTab ActiveTab
    | ProjectTemplateTypeHintMsg (TypeHint.Msg Project)
    | KnowledgeModelTypeHintMsg (TypeHint.Msg KnowledgeModelPackageSuggestion)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        validationMode =
            mapMode model
                ProjectCreateForm.TemplateValidationMode
                ProjectCreateForm.PackageValidationMode

        withDefaultMode ( m, c ) =
            let
                updated =
                    updateDefaultMode appState m
            in
            if countsPending model && not (countsPending updated) then
                ( updated, Cmd.batch [ c, tour appState updated ] )

            else
                ( updated, c )
    in
    case msg of
        GetSelectedProjectTemplateCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value record -> { record | selectedProjectTemplate = ActionResult.map .data value }
                , defaultError = gettext "Unable to get selected project template." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetSelectedKnowledgeModelCompleted result ->
            case result of
                Ok kmDetail ->
                    let
                        form =
                            ElmForm.update (ProjectCreateForm.validation validationMode)
                                (ElmForm.Input "language" ElmForm.Text (Form.Field.String kmDetail.language))
                                model.form
                    in
                    ( { model | selectedKnowledgeModel = ActionResult.Success kmDetail, form = form }, Cmd.none )

                Err error ->
                    ( { model | selectedKnowledgeModel = ApiError.toActionResult appState (gettext "Unable to get selected knowledge model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GetProjectTemplatesCountCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = \value record -> { record | anyProjectTemplates = value }
                , defaultError = gettext "Unable to get project templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                , locale = appState.locale
                }
                |> withDefaultMode

        GetKnowledgeModelsCountCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = \value record -> { record | anyKnowledgeModels = value }
                , defaultError = gettext "Unable to get knowledge models." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = \pagination -> pagination.page.totalElements > 0
                , locale = appState.locale
                }
                |> withDefaultMode

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState (Routes.projectsIndex appState)) )

        FormMsg formMsg ->
            let
                formOutput =
                    -- Validate first, so the output is the one of the current mode.
                    ElmForm.getOutput (ElmForm.update (ProjectCreateForm.validation validationMode) ElmForm.Validate model.form)
            in
            case ( formMsg, formOutput ) of
                ( ElmForm.Submit, Just form ) ->
                    let
                        ( body, request ) =
                            mapMode model
                                ( ProjectCreateForm.encodeFromTemplate form, ProjectsApi.postFromTemplate )
                                ( ProjectCreateForm.encodeFromPackage
                                    (if model.useAllQuestions then
                                        []

                                     else
                                        model.selectedTags
                                    )
                                    form
                                , ProjectsApi.post
                                )
                    in
                    ( { model | savingProject = ActionResult.Loading }
                    , request appState body (cfg.wrapMsg << PostProjectCompleted)
                    )

                _ ->
                    let
                        newModel =
                            { model | form = ElmForm.update (ProjectCreateForm.validation validationMode) formMsg model.form }

                        selectedPackage =
                            Maybe.andThen String.toMaybe (ElmForm.getFieldAsString "knowledgeModelPackageUuid" newModel.form).value
                    in
                    case selectedPackage of
                        Just kmPackageUuid ->
                            if newModel.lastFetchedPreview /= Just kmPackageUuid then
                                ( { newModel
                                    | lastFetchedPreview = Just kmPackageUuid
                                    , knowledgeModelPreview = ActionResult.Loading
                                    , selectedKnowledgeModel = ActionResult.Loading
                                    , selectedTags = []
                                  }
                                , Cmd.batch
                                    [ KnowledgeModelsApi.fetchPreview appState (Just (Uuid.fromUuidString kmPackageUuid)) [] [] (cfg.wrapMsg << GetKnowledgeModelPreviewCompleted)
                                    , KnowledgeModelPackagesApi.getKnowledgeModelPackage appState (Uuid.fromUuidString kmPackageUuid) (cfg.wrapMsg << GetSelectedKnowledgeModelCompleted)
                                    ]
                                )

                            else
                                ( newModel, Cmd.none )

                        Nothing ->
                            ( { newModel
                                | lastFetchedPreview = Nothing
                                , knowledgeModelPreview = ActionResult.Unset
                                , selectedKnowledgeModel = ActionResult.Unset
                                , selectedTags = []
                              }
                            , Cmd.none
                            )

        PostProjectCompleted result ->
            case result of
                Ok project ->
                    ( model, Routes.navigate appState (Routes.projectsDetail project.uuid) )

                Err error ->
                    ( { model | savingProject = ApiError.toActionResult appState (gettext "Project could not be created." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , Form.focusErrorSummary
                        ]
                    )

        ToggleTag tagUuid ->
            if List.member tagUuid model.selectedTags then
                ( { model | selectedTags = List.filter ((/=) tagUuid) model.selectedTags }, Cmd.none )

            else
                ( { model | selectedTags = tagUuid :: model.selectedTags }, Cmd.none )

        ChangeUseAllQuestions value ->
            ( { model | useAllQuestions = value }, Cmd.none )

        GetKnowledgeModelPreviewCompleted result ->
            case result of
                Ok knowledgeModel ->
                    ( { model | knowledgeModelPreview = ActionResult.Success knowledgeModel }, Cmd.none )

                Err error ->
                    ( { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the knowledge model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetActiveTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        ProjectTemplateTypeHintMsg typeHintMsg ->
            let
                formMsg =
                    cfg.wrapMsg << FormMsg << ElmForm.Input "projectUuid" ElmForm.Select << Form.Field.String

                ( projectTemplateTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << ProjectTemplateTypeHintMsg
                        , getTypeHints = getProjectTemplates appState << projectTemplatesTypeHintsQueryString
                        , getError = gettext "Unable to get project templates." appState.locale
                        , setReply = formMsg << Uuid.toString << .uuid
                        , clearReply = Just (formMsg "")
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = projectTemplateId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.projectTemplateTypeHint
            in
            ( { model | projectTemplateTypeHint = projectTemplateTypeHint }, cmd )

        KnowledgeModelTypeHintMsg typeHintMsg ->
            let
                formMsg =
                    cfg.wrapMsg << FormMsg << ElmForm.Input "knowledgeModelPackageUuid" ElmForm.Select << Form.Field.String

                ( knowledgeModelTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << KnowledgeModelTypeHintMsg
                        , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
                        , getError = gettext "Unable to get knowledge models." appState.locale
                        , setReply = formMsg << Uuid.toString << .uuid
                        , clearReply = Just (formMsg "")
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = knowledgeModelId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.knowledgeModelTypeHint
            in
            ( { model | knowledgeModelTypeHint = knowledgeModelTypeHint }, cmd )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Form.submitShortcut appState { result = model.savingProject, onSubmit = FormMsg ElmForm.Submit }
        , Sub.map ProjectTemplateTypeHintMsg (TypeHint.subscriptions model.projectTemplateTypeHint)
        , Sub.map KnowledgeModelTypeHintMsg (TypeHint.subscriptions model.knowledgeModelTypeHint)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        -- In the default mode a knowledge model chosen in the form reloads
        -- `selectedKnowledgeModel`, and that must not take the form away from
        -- under the field that has focus, so only a preselected one is waited
        -- for.
        selected =
            case model.mode of
                FromProjectTemplateMode ->
                    [ ActionResult.map (always ()) model.selectedProjectTemplate ]

                FromKnowledgeModelMode ->
                    [ ActionResult.map (always ()) model.selectedKnowledgeModel ]

                DefaultMode _ ->
                    []

        actionResult =
            (selected
                ++ [ ActionResult.map (always ()) model.anyProjectTemplates
                   , ActionResult.map (always ()) model.anyKnowledgeModels
                   ]
            )
                |> List.filterNot ActionResult.isUnset
                |> ActionResult.all
    in
    if ActionResult.isUnset actionResult then
        viewForm appState model

    else
        Page.actionResult appState (always (viewForm appState model)) actionResult


viewForm : AppState -> Model -> Html Msg
viewForm appState model =
    Form.createPage appState
        { title = gettext "Create Project" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsCreate ]
        , content =
            notMemberWarning appState
                ++ Html.map FormMsg
                    (FormField.input appState model.form "name" (gettext "Name" appState.locale)
                        |> FormField.withRequired True
                        |> FormField.toHtml
                    )
                :: formContent appState model
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingProject
        , onSubmit = FormMsg ElmForm.Submit
        , onCancel = Just Cancel
        }


{-| Someone who is in the workspace through their organization role alone
makes a project they are not a member of the workspace of, so they
hear it before they make it.
-}
notMemberWarning : AppState -> List (Html msg)
notMemberWarning appState =
    if appState.workspaceMember then
        []

    else
        [ Alert.toHtml (Alert.warning (gettext "You are not a member of this workspace. The project will be created here all the same, and you will reach it through your role in the organization." appState.locale)) ]


formContent : AppState -> Model -> List (Html Msg)
formContent appState model =
    case model.mode of
        FromProjectTemplateMode ->
            case model.selectedProjectTemplate of
                ActionResult.Success projectTemplate ->
                    [ Field.readOnly
                        { id = projectTemplateId
                        , label = gettext "Project Template" appState.locale
                        , value = projectTemplate.name
                        }
                        |> Field.toHtml
                    ]

                _ ->
                    []

        FromKnowledgeModelMode ->
            case model.selectedKnowledgeModel of
                ActionResult.Success packageDetail ->
                    let
                        knowledgeModelField_ =
                            Field.readOnly
                                { id = knowledgeModelId
                                , label = gettext "Knowledge Model" appState.locale
                                , value = packageDetail.name ++ " (" ++ KnowledgeModelUtils.getPackageId (KnowledgeModelPackageDetail.toPackageSuggestion packageDetail) ++ ")"
                                }
                                |> Field.toHtml
                    in
                    knowledgeModelField_ :: languageView appState model ++ tagsView appState model

                _ ->
                    []

        DefaultMode defaultMode ->
            (if defaultMode == TabsDefaultMode then
                [ startingPointView appState model ]

             else
                []
            )
                ++ mapMode model
                    [ projectTemplateField appState model ]
                    (knowledgeModelField appState model
                        :: languageView appState model
                        ++ tagsView appState model
                    )


startingPointView : AppState -> Model -> Html Msg
startingPointView appState model =
    let
        toValue tab =
            case tab of
                ProjectTemplateTab ->
                    "template"

                KnowledgeModelTab ->
                    "knowledge-model"

        fromValue value =
            if value == toValue KnowledgeModelTab then
                KnowledgeModelTab

            else
                ProjectTemplateTab
    in
    Field.radioGroup
        { id = startingPointId
        , label = gettext "Starting Point" appState.locale
        , value = toValue model.activeTab
        , options =
            [ Field.choice { value = toValue ProjectTemplateTab, label = gettext "From project template" appState.locale }
                |> Field.withChoiceDescription (gettext "Project templates are prepared projects with knowledge models, question tags, and document templates setup, so you don't have to start from scratch." appState.locale)
            , Field.choice { value = toValue KnowledgeModelTab, label = gettext "From knowledge model" appState.locale }
                |> Field.withChoiceDescription (gettext "Knowledge models are templates for questionnaires. You can select question tags now and configure the document template and other settings later." appState.locale)
            ]
        , onChange = SetActiveTab << fromValue
        }
        |> Field.toHtml


fieldError : AppState -> Form FormError o -> String -> String -> Maybe String
fieldError appState form path label =
    FormField.errorText appState label (ElmForm.getFieldAsString path form)


projectTemplateField : AppState -> Model -> Html Msg
projectTemplateField appState model =
    let
        label =
            gettext "Project Template" appState.locale
    in
    TypeHint.view appState
        { id = projectTemplateId
        , label = label
        , placeholder = gettext "Select project template" appState.locale
        , required = True
        , error = fieldError appState model.form "projectUuid" label
        , hint = Nothing
        , clearable = True
        , viewItem = \project -> { label = project.name, code = Nothing, description = project.description, icon = Nothing, badges = [] }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = ProjectTemplateTypeHintMsg
        }
        model.projectTemplateTypeHint


knowledgeModelField : AppState -> Model -> Html Msg
knowledgeModelField appState model =
    let
        label =
            gettext "Knowledge Model" appState.locale
    in
    TypeHint.view appState
        { id = knowledgeModelId
        , label = label
        , placeholder = gettext "Select knowledge model" appState.locale
        , required = True
        , error = fieldError appState model.form "knowledgeModelPackageUuid" label
        , hint = Nothing
        , clearable = True
        , viewItem =
            \kmPackage ->
                { label = kmPackage.name
                , code = Just (KnowledgeModelUtils.getPackageId kmPackage)
                , badges = RowBadge.owner appState kmPackage.workspaceUuid
                , description = Just kmPackage.description
                , icon = Nothing
                }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = KnowledgeModelTypeHintMsg
        }
        model.knowledgeModelTypeHint


languageView : AppState -> Model -> List (Html Msg)
languageView appState model =
    case model.selectedKnowledgeModel of
        ActionResult.Success kmDetail ->
            if List.isEmpty kmDetail.locales then
                []

            else
                let
                    options =
                        ( kmDetail.language, kmDetail.language )
                            :: List.map (\locale -> ( locale.code, locale.code )) (List.sortBy .code kmDetail.locales)
                in
                [ Html.map FormMsg
                    (FormField.select appState model.form "language" (gettext "Project Language" appState.locale) options
                        |> FormField.toHtml
                    )
                ]

        _ ->
            []


tagsView : AppState -> Model -> List (Html Msg)
tagsView appState model =
    case model.knowledgeModelPreview of
        ActionResult.Unset ->
            []

        ActionResult.Loading ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]

        ActionResult.Error error ->
            [ Alert.toHtml (Alert.danger error) ]

        ActionResult.Success knowledgeModel ->
            case KnowledgeModel.getTags knowledgeModel of
                [] ->
                    [ Alert.toHtml (Alert.info (gettext "No need to choose question tags for this knowledge model." appState.locale)) ]

                tags ->
                    let
                        modeField =
                            Field.radioGroup
                                { id = "project-create-question-tags-mode"
                                , label = gettext "Question Tags" appState.locale
                                , value = Bool.toString model.useAllQuestions
                                , options =
                                    [ Field.choice { value = Bool.toString True, label = gettext "Use all questions" appState.locale }
                                    , Field.choice { value = Bool.toString False, label = gettext "Filter by question tags" appState.locale }
                                    ]
                                , onChange = ChangeUseAllQuestions << (==) (Bool.toString True)
                                }
                                |> Field.withHint (gettext "You can either use all questions from the knowledge model or filter them by question tags." appState.locale)
                                |> Field.toHtml
                    in
                    -- The same structure either way, so the radio that was just
                    -- pressed is not rebuilt from under the keyboard.
                    [ Layout.stack
                        [ modeField
                        , if model.useAllQuestions then
                            Html.text ""

                          else
                            tagsSelection appState model tags
                        ]
                    ]


tagsSelection : AppState -> Model -> List Tag -> Html Msg
tagsSelection appState model tags =
    let
        tagChoice tag =
            let
                choice =
                    Field.choice
                        { value = tag.uuid
                        , label =
                            if String.isEmpty tag.name then
                                gettext "Untitled question tag" appState.locale

                            else
                                tag.name
                        }
                        |> Field.withChoiceColor tag.color
            in
            case tag.description of
                Just description ->
                    Field.withChoiceDescription description choice

                Nothing ->
                    choice
    in
    Field.checkboxGroup
        { id = "project-create-question-tags"
        , label = gettext "Filter by question tags" appState.locale
        , values = model.selectedTags
        , options = List.map tagChoice (List.sortBy .name tags)
        , onToggle = ToggleTag
        }
        |> Field.withHiddenLabel
        |> Field.toHtml
