module Common.Pages.Projects.CreateMigration exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Moving a project onto another knowledge model.

One column of two groups: what the project is on now, and under it what it is to
be moved to. The first is read only — it is the project as it stands — and the
second is the choice: a knowledge model, one of its versions, and which of its
question tags to bring. They are not side by side: a form is read down, and a
`Ui.Form.section` is how a form here says that some of its fields belong
together.

The page loads in a chain: the project's settings say which knowledge model it
is on; that package says which versions it has, and its
latest is chosen to start with; that version's preview says which question tags
there are to choose from. Choosing another knowledge model, or another version
of one, starts the chain again from where it changed.

The answers already given are not migrated by this page — the server does that,
and the alert above the form says what may not survive. On success the project
that comes back is opened, which is the migration, not the project this was
started from.

**Comparing the two knowledge models is not there yet.** The button is here and
opens a dialog that says so.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Common.Api.Models.VersionUuid as VersionUuid
import Common.Api.Projects as ProjectsApi
import Common.Components.Form as Form
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.KMComparison as KMComparison
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.Forms.ProjectMigrationCreateForm as ProjectMigrationCreateForm exposing (ProjectMigrationCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Bool as Bool
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form as ElmForm exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Field as Field
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Modal as Modal
import Ui.Tag as UiTag
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { projectUuid : Uuid
    , project : ActionResult ProjectSettings
    , currentPackage : ActionResult KnowledgeModelPackageDetail
    , selectedPackage : Maybe KnowledgeModelPackageSuggestion
    , selectedPackageDetail : ActionResult KnowledgeModelPackageDetail
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastPreviewedVersion : Maybe Uuid
    , selectedTags : List String
    , useAllQuestions : Bool
    , form : Form FormError ProjectMigrationCreateForm
    , savingMigration : ActionResult String
    , knowledgeModelTypeHint : TypeHint.Model KnowledgeModelPackageSuggestion
    , compareOpen : Bool
    , comparison : KMComparison.Model
    }


initialModel : AppState -> Uuid -> Model
initialModel appState projectUuid =
    { projectUuid = projectUuid
    , project = Loading
    , currentPackage = Loading
    , selectedPackage = Nothing
    , selectedPackageDetail = Loading
    , knowledgeModelPreview = Unset
    , lastPreviewedVersion = Nothing
    , selectedTags = []
    , useAllQuestions = True
    , form = ProjectMigrationCreateForm.initEmpty
    , savingMigration = Unset
    , knowledgeModelTypeHint = TypeHint.init
    , compareOpen = False
    , comparison = KMComparison.initialModel appState comparisonId
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState projectUuid =
    ( initialModel appState projectUuid
    , ProjectsApi.getSettings appState projectUuid GotProject
    )


knowledgeModelId : String
knowledgeModelId =
    "project-migration-knowledge-model"


comparisonId : String
comparisonId =
    "project-migration-comparison"


{-| The version the form holds, when it holds one.
-}
selectedVersionUuid : Model -> Maybe Uuid
selectedVersionUuid model =
    case Maybe.map .knowledgeModelPackageUuid (ElmForm.getOutput model.form) of
        Just value ->
            if String.isEmpty value then
                Nothing

            else
                Just (Uuid.fromUuidString value)

        Nothing ->
            Nothing



-- UPDATE


type Msg
    = GotProject (Result ApiError (ProjectDetailWrapper ProjectSettings))
    | GotCurrentPackage (Result ApiError KnowledgeModelPackageDetail)
    | GotSelectedPackage (Result ApiError KnowledgeModelPackageDetail)
    | GotPreview (Result ApiError KnowledgeModel)
    | GotMigration (Result ApiError ProjectCommon)
    | SelectPackage KnowledgeModelPackageSuggestion
    | KnowledgeModelTypeHintMsg (TypeHint.Msg KnowledgeModelPackageSuggestion)
    | FormMsg ElmForm.Msg
    | ChangeUseAllQuestions Bool
    | ToggleTag String
    | OpenCompare
    | CloseCompare
    | ComparisonMsg KMComparison.Msg
    | Cancel


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        plain ( newModel, cmd ) =
            ( newModel, Cmd.map cfg.wrapMsg cmd )
    in
    case msg of
        GotProject result ->
            case result of
                Ok wrapper ->
                    if not (Feature.projectCreateMigration appState wrapper.common) then
                        ( model, UpdateConfig.notAllowed cfg )

                    else
                        let
                            project =
                                wrapper.data
                        in
                        plain
                            ( { model
                                | project = Success project

                                -- The tags the project is on now are where the
                                -- choice starts, as the wizard had it.
                                , selectedTags = project.selectedQuestionTagUuids
                                , useAllQuestions = List.isEmpty project.selectedQuestionTagUuids
                              }
                            , KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState project.knowledgeModelPackage.uuid GotCurrentPackage
                            )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.project appState)
                        )

                    else
                        ( { model | project = ApiError.toActionResult appState (gettext "Unable to get the project." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        GotCurrentPackage result ->
            case result of
                Ok packageDetail ->
                    -- The knowledge model the project is on is also the one it
                    -- is moved to until another is chosen: its newest version
                    -- is the migration somebody came here to make.
                    plain
                        (preselect appState
                            { model
                                | currentPackage = Success packageDetail
                                , selectedPackageDetail = Success packageDetail
                            }
                            packageDetail
                        )

                Err error ->
                    ( { model
                        | currentPackage = ApiError.toActionResult appState (gettext "Unable to get the knowledge model." appState.locale) error
                        , selectedPackageDetail = ApiError.toActionResult appState (gettext "Unable to get the knowledge model." appState.locale) error
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GotSelectedPackage result ->
            case result of
                Ok packageDetail ->
                    plain (preselect appState { model | selectedPackageDetail = Success packageDetail } packageDetail)

                Err error ->
                    ( { model | selectedPackageDetail = ApiError.toActionResult appState (gettext "Unable to get the knowledge model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GotPreview result ->
            case result of
                Ok knowledgeModel ->
                    plain ( { model | knowledgeModelPreview = Success knowledgeModel }, Cmd.none )

                Err error ->
                    ( { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the knowledge model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GotMigration result ->
            case result of
                Ok project ->
                    -- The migration is a project of its own, and it is the one
                    -- to open: the project this was started from stays as it is
                    -- until the migration is finished.
                    ( model, Routes.navigate appState (Routes.projectsDetail project.uuid) )

                Err error ->
                    ( { model | savingMigration = ApiError.toActionResult appState (gettext "Project migration could not be created." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SelectPackage package ->
            plain
                ( { model
                    | selectedPackage = Just package
                    , selectedPackageDetail = Loading
                    , knowledgeModelPreview = Unset
                    , lastPreviewedVersion = Nothing
                    , selectedTags = []
                    , form = ElmForm.update ProjectMigrationCreateForm.validation (ElmForm.Input "knowledgeModelPackageUuid" ElmForm.Select Form.Field.EmptyField) model.form
                  }
                , KnowledgeModelPackagesApi.getKnowledgeModelPackageWithoutDeprecatedVersions appState package.uuid GotSelectedPackage
                )

        KnowledgeModelTypeHintMsg typeHintMsg ->
            let
                ( knowledgeModelTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << KnowledgeModelTypeHintMsg
                        , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState Nothing
                        , getError = gettext "Unable to get knowledge models." appState.locale
                        , setReply = cfg.wrapMsg << SelectPackage
                        , clearReply = Nothing
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = knowledgeModelId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.knowledgeModelTypeHint
            in
            ( { model | knowledgeModelTypeHint = knowledgeModelTypeHint }, cmd )

        FormMsg formMsg ->
            case ( formMsg, ElmForm.getOutput model.form ) of
                ( ElmForm.Submit, Just form ) ->
                    let
                        tagUuids =
                            chosenTags model
                    in
                    plain
                        ( { model | savingMigration = Loading }
                        , ProjectsApi.fetchMigration appState model.projectUuid (ProjectMigrationCreateForm.encode tagUuids form) GotMigration
                        )

                _ ->
                    let
                        updated =
                            { model | form = ElmForm.update ProjectMigrationCreateForm.validation formMsg model.form }

                        -- The tags on offer are the chosen version's, so a
                        -- change of version asks for them again — and what was
                        -- ticked under the old version means nothing under the
                        -- new one.
                        version =
                            selectedVersionUuid updated
                    in
                    if Maybe.isJust version && version /= updated.lastPreviewedVersion then
                        plain (fetchPreview appState { updated | selectedTags = [] } version)

                    else
                        plain ( updated, Cmd.none )

        ChangeUseAllQuestions useAll ->
            plain ( { model | useAllQuestions = useAll }, Cmd.none )

        ToggleTag tagUuid ->
            let
                selectedTags =
                    if List.member tagUuid model.selectedTags then
                        List.remove tagUuid model.selectedTags

                    else
                        tagUuid :: model.selectedTags
            in
            plain ( { model | selectedTags = selectedTags }, Cmd.none )

        OpenCompare ->
            -- What the project is on now against what it is being moved to,
            -- each with its own question tags: the project's on the left, the
            -- ones chosen here on the right.
            case ( model.project, model.currentPackage, model.selectedPackageDetail ) of
                ( Success project, Success currentPackage, Success selectedPackage ) ->
                    case selectedVersionUuid model of
                        Just targetVersion ->
                            updateComparison cfg
                                appState
                                (KMComparison.compare
                                    { leftPackage = currentPackage
                                    , rightPackage = selectedPackage
                                    , leftVersion = project.knowledgeModelPackage.uuid
                                    , rightVersion = targetVersion
                                    , leftTags = project.selectedQuestionTagUuids
                                    , rightTags = chosenTags model
                                    }
                                )
                                { model | compareOpen = True }

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseCompare ->
            plain ( { model | compareOpen = False }, Cmd.none )

        ComparisonMsg comparisonMsg ->
            updateComparison cfg appState comparisonMsg model

        Cancel ->
            plain ( model, Window.historyBack (Routes.toUrl appState (Routes.projectsIndex appState)) )


updateComparison : UpdateConfig Msg msg -> AppState -> KMComparison.Msg -> Model -> ( Model, Cmd msg )
updateComparison cfg appState comparisonMsg model =
    let
        ( comparison, cmd ) =
            KMComparison.update appState
                { wrapMsg = cfg.wrapMsg << ComparisonMsg
                , logoutMsg = cfg.logoutMsg
                }
                comparisonMsg
                model.comparison
    in
    ( { model | comparison = comparison }, cmd )


{-| The tags the migration is made with: none at all when every question is kept.
-}
chosenTags : Model -> List String
chosenTags model =
    if model.useAllQuestions then
        []

    else
        model.selectedTags


{-| A knowledge model has just been loaded: its newest version is put in the
form and in the field, and that version's question tags are asked for.
-}
preselect : AppState -> Model -> KnowledgeModelPackageDetail -> ( Model, Cmd Msg )
preselect appState model packageDetail =
    let
        latest =
            KnowledgeModelPackageDetail.getLatestPackageUuid packageDetail

        withForm =
            case latest of
                Just versionUuid ->
                    { model | form = ProjectMigrationCreateForm.init versionUuid }

                Nothing ->
                    model

        withSelected =
            { withForm
                | knowledgeModelTypeHint =
                    TypeHint.setSelected
                        (Just (KnowledgeModelPackageDetail.toPackageSuggestion packageDetail))
                        withForm.knowledgeModelTypeHint
                , selectedPackage = Just (KnowledgeModelPackageDetail.toPackageSuggestion packageDetail)
            }
    in
    fetchPreview appState withSelected latest


fetchPreview : AppState -> Model -> Maybe Uuid -> ( Model, Cmd Msg )
fetchPreview appState model mbVersionUuid =
    case mbVersionUuid of
        Just versionUuid ->
            ( { model | knowledgeModelPreview = Loading, lastPreviewedVersion = Just versionUuid }
            , KnowledgeModelsApi.fetchPreview appState (Just versionUuid) [] [] GotPreview
            )

        Nothing ->
            ( { model | knowledgeModelPreview = Unset, lastPreviewedVersion = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Form.submitShortcut appState { result = model.savingMigration, onSubmit = FormMsg ElmForm.Submit }
        , Sub.map KnowledgeModelTypeHintMsg (TypeHint.subscriptions model.knowledgeModelTypeHint)
        , Sub.map ComparisonMsg (KMComparison.subscriptions model.comparison)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewForm appState model) model.project


viewForm : AppState -> Model -> ProjectSettings -> Html Msg
viewForm appState model project =
    Form.createPage appState
        { title = gettext "Migrate Project" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsMigration ]
        , content =
            [ Alert.toHtml (Alert.warning (gettext "This will update the project’s knowledge model. Replies associated with questions that aren’t included in the new knowledge model may not be carried over." appState.locale))
            , Field.toHtml
                (Field.readOnly
                    { id = "project-migration-project"
                    , label = gettext "Project" appState.locale
                    , value = project.name
                    }
                )
            , UiForm.section
                { id = "project-migration-current"
                , legend = gettext "Current Knowledge Model" appState.locale
                , content = viewCurrent appState project
                }
            , Html.div [ class "c-project-migration__arrow" ] [ Icon.toHtml (Icon.solid "fa-arrow-down") ]
            , UiForm.section
                { id = "project-migration-target"
                , legend = gettext "New Knowledge Model" appState.locale
                , content = viewTarget appState model
                }
            , viewCompareModal appState model
            ]
        , submitLabel = gettext "Migrate" appState.locale
        , result = model.savingMigration
        , onSubmit = FormMsg ElmForm.Submit
        , onCancel = Just Cancel
        }


{-| What the project is on now: nothing here can be changed. Each field says
"Original", which is what tells it from the choice under it.
-}
viewCurrent : AppState -> ProjectSettings -> List (Html Msg)
viewCurrent appState project =
    [ Field.toHtml
        (Field.readOnly
            { id = "project-migration-current-knowledge-model"
            , label = gettext "Knowledge Model" appState.locale
            , value = project.knowledgeModelPackage.name
            }
        )
    , Field.toHtml
        (Field.readOnly
            { id = "project-migration-current-version"
            , label = gettext "Version" appState.locale
            , value = Version.toString project.knowledgeModelPackage.version
            }
        )
    , -- A field of its own, not a label with something under it: the tags are
      -- this field's value, and a label sits closer to its value than two
      -- things in a group sit to each other.
      Field.toHtml
        (Field.readOnlyHtml
            { id = "project-migration-current-tags"
            , label = gettext "Question tags" appState.locale
            , value = [ viewCurrentTags appState project ]
            }
        )
    ]


viewCurrentTags : AppState -> ProjectSettings -> Html Msg
viewCurrentTags appState project =
    if List.isEmpty project.knowledgeModelTags then
        Text.muted (gettext "There are no question tags for this knowledge model." appState.locale)

    else
        let
            chosen =
                List.filter (\tag -> List.member tag.uuid project.selectedQuestionTagUuids) project.knowledgeModelTags
        in
        if List.isEmpty chosen then
            Text.muted (gettext "All questions are used." appState.locale)

        else
            UiTag.list (List.map (viewTag appState) (List.sortBy .name chosen))


viewTag : AppState -> Tag -> UiTag.Tag Msg
viewTag appState tag =
    UiTag.tag (tagName appState tag)
        |> UiTag.withColor tag.color


tagName : AppState -> Tag -> String
tagName appState tag =
    if String.isEmpty tag.name then
        gettext "Untitled question tag" appState.locale

    else
        tag.name


{-| What it is to be moved to: the knowledge model, its version, and which of
its questions to bring.
-}
viewTarget : AppState -> Model -> List (Html Msg)
viewTarget appState model =
    TypeHint.view appState
        { id = knowledgeModelId
        , label = gettext "Knowledge Model" appState.locale
        , placeholder = gettext "Select knowledge model" appState.locale
        , required = True
        , error = Nothing
        , hint = Nothing
        , clearable = False
        , viewItem =
            \kmPackage ->
                -- No package id: it ends in a version, and the version is the
                -- next field down.
                { label = kmPackage.name
                , code = Nothing
                , badges = RowBadge.owner appState kmPackage.workspaceUuid
                , description = Just kmPackage.description
                , icon = Nothing
                }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = KnowledgeModelTypeHintMsg
        }
        model.knowledgeModelTypeHint
        :: viewVersion appState model
        ++ viewTags appState model
        ++ viewCompareButton appState model


viewVersion : AppState -> Model -> List (Html Msg)
viewVersion appState model =
    let
        label =
            gettext "Version" appState.locale
    in
    case ( model.selectedPackage, model.selectedPackageDetail ) of
        ( Nothing, _ ) ->
            [ Field.toHtml
                (Field.readOnly
                    { id = "project-migration-version"
                    , label = label
                    , value = gettext "Select knowledge model first" appState.locale
                    }
                )
            ]

        ( Just _, Success packageDetail ) ->
            let
                options =
                    ( "", "--" )
                        :: List.map
                            (\version -> ( Uuid.toString version.uuid, Version.toString version.version ))
                            (List.reverse (List.sortWith VersionUuid.compare packageDetail.versions))
            in
            [ Html.map FormMsg
                (FormField.select appState model.form "knowledgeModelPackageUuid" label options
                    |> FormField.withRequired True
                    |> FormField.toHtml
                )
            ]

        ( Just _, Error error ) ->
            [ Alert.toHtml (Alert.danger error) ]

        ( Just _, _ ) ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]


viewTags : AppState -> Model -> List (Html Msg)
viewTags appState model =
    case model.knowledgeModelPreview of
        Unset ->
            []

        Loading ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]

        Error error ->
            [ Alert.toHtml (Alert.danger error) ]

        Success knowledgeModel ->
            case KnowledgeModel.getTags knowledgeModel of
                [] ->
                    [ Alert.toHtml (Alert.info (gettext "No need to choose question tags for this knowledge model." appState.locale)) ]

                tags ->
                    [ Layout.stack
                        [ Field.toHtml
                            (Field.radioGroup
                                { id = "project-migration-question-tags-mode"
                                , label = gettext "Question Tags" appState.locale
                                , value = Bool.toString model.useAllQuestions
                                , options =
                                    [ Field.choice { value = Bool.toString True, label = gettext "Use all questions" appState.locale }
                                    , Field.choice { value = Bool.toString False, label = gettext "Filter by question tags" appState.locale }
                                    ]
                                , onChange = ChangeUseAllQuestions << (==) (Bool.toString True)
                                }
                                |> Field.withHint (gettext "You can either use all questions from the knowledge model or filter them by question tags." appState.locale)
                            )
                        , if model.useAllQuestions then
                            Html.text ""

                          else
                            viewTagsSelection appState model tags
                        ]
                    ]


viewTagsSelection : AppState -> Model -> List Tag -> Html Msg
viewTagsSelection appState model tags =
    let
        tagChoice tag =
            let
                choice =
                    Field.choice { value = tag.uuid, label = tagName appState tag }
                        |> Field.withChoiceColor tag.color
            in
            case tag.description of
                Just description ->
                    Field.withChoiceDescription description choice

                Nothing ->
                    choice
    in
    Field.toHtml
        (Field.checkboxGroup
            { id = "project-migration-question-tags"
            , label = gettext "Filter by question tags" appState.locale
            , values = model.selectedTags
            , options = List.map tagChoice (List.sortBy .name tags)
            , onToggle = ToggleTag
            }
            |> Field.withHiddenLabel
        )


{-| The way to the comparison of the two knowledge models, offered once there is
something to compare with — which is what the preview having arrived means.
-}
viewCompareButton : AppState -> Model -> List (Html Msg)
viewCompareButton appState model =
    if ActionResult.isSuccess model.knowledgeModelPreview then
        [ Button.toHtml
            (Button.secondary (gettext "Compare" appState.locale) OpenCompare
                |> Button.withIcon (Icon.solid "fa-code-compare")
                |> Button.withTestId "project-migration_compare"
            )
        ]

    else
        []


viewCompareModal : AppState -> Model -> Html Msg
viewCompareModal appState model =
    Modal.plain
        { title = gettext "Compare Knowledge Models" appState.locale
        , open = model.compareOpen
        , onClose = CloseCompare
        , body = [ Html.map ComparisonMsg (KMComparison.view appState model.comparison) ]
        }
        |> Modal.withCover
        -- A page's worth of comparison, so the room a footer would take goes to
        -- the table and the way out is in the header instead — as the version
        -- view modal does with a questionnaire.
        |> Modal.withActions []
        |> Modal.withHeaderAction
            (Button.iconOnly (Icon.solid "fa-xmark") (gettext "Close" appState.locale) CloseCompare
                |> Button.quiet
            )
        |> Modal.withTestId "project-migration_compare-modal"
        |> Modal.toHtml
