module WizardResearch.Pages.ProjectCreate exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Css exposing (listStyleType, none)
import Form exposing (Form)
import Form.Field as Field exposing (FieldValue)
import Html.Styled as Html exposing (Html, div, h1, h2, h3, li, span, text, ul)
import Html.Styled.Attributes exposing (autocomplete, css, disabled)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Template as Template exposing (Template)
import Shared.Data.Template.TemplatePacakge as TemplatePackage
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Atoms.Form as Form
import Shared.Elemental.Atoms.FormInput as FormInput
import Shared.Elemental.Atoms.Heading as Heading
import Shared.Elemental.Atoms.Text as Text
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Components.Carousel as Carousel exposing (PageOptions)
import Shared.Elemental.Components.FileIconList as FileIconList
import Shared.Elemental.Components.ProgressBar as ProgressBar
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Elemental.Foundations.Grid as Grid exposing (Grid)
import Shared.Elemental.Foundations.Illustration as Illustration
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html.Styled exposing (emptyNode, fa)
import Task
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Pages.ProjectCreate.KnowledgeModelModal as KnowledgeModelModal
import WizardResearch.Pages.ProjectCreate.ProjectCreateForm as ProjectCreateForm exposing (ProjectCreateForm)
import WizardResearch.Route as Route exposing (Route)
import WizardResearch.Route.ProjectRoute as ProjectRoute



-- MODEL


type alias Model =
    { screen : Screen
    , createForm : Form FormError ProjectCreateForm
    , templates : ActionResult (List Template)
    , knowledgeModel : ActionResult KnowledgeModel
    , submitting : ActionResult ()
    , templateListExpanded : Bool
    , kmListExpanded : Bool
    , kmModalModel : KnowledgeModelModal.Model
    }


type Screen
    = NameScreen
    | TemplateScreen
    | KnowledgeModelScreen


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { screen = NameScreen
      , createForm = ProjectCreateForm.init
      , templates = Loading
      , knowledgeModel = Unset
      , submitting = Unset
      , templateListExpanded = False
      , kmListExpanded = False
      , kmModalModel = KnowledgeModelModal.init
      }
    , TemplatesApi.getTemplates appState GetTemplatesComplete
    )


getTags : Model -> List Tag
getTags =
    .knowledgeModel
        >> ActionResult.map KnowledgeModel.getTags
        >> ActionResult.withDefault []


getSelectedTemplate : Model -> Maybe Template
getSelectedTemplate model =
    case model.templates of
        Success templates ->
            (Form.getFieldAsString "templateUuid" model.createForm).value
                |> Maybe.andThen (Template.findByUuid templates)

        _ ->
            Nothing


getProjectName : Model -> String
getProjectName model =
    Maybe.withDefault "" (Form.getFieldAsString "name" model.createForm).value



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | SetScreen Screen
    | GetTemplatesComplete (Result ApiError (List Template))
    | GetKnowledgeModelComplete (Result ApiError KnowledgeModel)
    | PostQuestionnaireComplete (Result ApiError Questionnaire)
    | ExpandTemplateList Bool
    | ExpandKMList Bool
    | KnowledgeModelModalMsg KnowledgeModelModal.Msg
    | NoOp


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , cmdNavigate : Route -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg cfg appState formMsg model

        SetScreen screen ->
            handleSetScreen cfg appState model screen

        GetTemplatesComplete result ->
            handleGetTemplatesComplete cfg appState model result

        GetKnowledgeModelComplete result ->
            handleGetKnowledgeModelComplete model result

        PostQuestionnaireComplete result ->
            handlePostQuestionnaireComplete cfg model result

        ExpandTemplateList expanded ->
            ( { model | templateListExpanded = expanded }, Cmd.none )

        ExpandKMList expanded ->
            ( { model | kmListExpanded = expanded }, Cmd.none )

        KnowledgeModelModalMsg kmModalMsg ->
            let
                ( kmModalModel, cmd ) =
                    KnowledgeModelModal.update appState kmModalMsg model.kmModalModel
            in
            ( { model | kmModalModel = kmModalModel }, Cmd.map (cfg.wrapMsg << KnowledgeModelModalMsg) cmd )

        _ ->
            ( model, Cmd.none )


handleFormMsg : UpdateConfig msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.createForm ) of
        ( Form.Submit, Just createForm ) ->
            ( { model | submitting = Loading }
            , QuestionnairesApi.postQuestionnaire (ProjectCreateForm.encode appState createForm) appState (cfg.wrapMsg << PostQuestionnaireComplete)
            )

        _ ->
            let
                createForm =
                    Form.update (ProjectCreateForm.validation (getTags model)) formMsg model.createForm

                -- Select recommended package for selected template
                createFormWithPackageIdSet =
                    case ( formMsg, model.templates ) of
                        ( Form.Input "templateUuid" _ _, Success templates ) ->
                            ProjectCreateForm.selectRecommendedPackage (getTags model) templates createForm

                        _ ->
                            createForm

                -- Fetch tags for selected package
                ( knowledgeModel, cmd ) =
                    case formMsg of
                        Form.Input "packageId" _ (Field.String packageId) ->
                            ( Loading, KnowledgeModelsApi.fetchPreview packageId appState (cfg.wrapMsg << GetKnowledgeModelComplete) )

                        _ ->
                            ( model.knowledgeModel, Cmd.none )
            in
            ( { model
                | createForm = createFormWithPackageIdSet
                , knowledgeModel = knowledgeModel
              }
            , cmd
            )


handleSetScreen : UpdateConfig msg -> AppState -> Model -> Screen -> ( Model, Cmd msg )
handleSetScreen cfg appState model screen =
    let
        -- Fetch tags for selected package when entering knowledge model screen
        ( knowledgeModel, cmd ) =
            case ( screen, (Form.getFieldAsString "packageId" model.createForm).value ) of
                ( KnowledgeModelScreen, Just packageId ) ->
                    ( Loading, KnowledgeModelsApi.fetchPreview packageId appState (cfg.wrapMsg << GetKnowledgeModelComplete) )

                _ ->
                    ( model.knowledgeModel, Cmd.none )
    in
    ( { model | screen = screen, knowledgeModel = knowledgeModel }, cmd )


handleGetTemplatesComplete : UpdateConfig msg -> AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd msg )
handleGetTemplatesComplete cfg appState model result =
    case result of
        Ok templates ->
            ( { model
                | templates = Success templates
                , createForm =
                    model.createForm
                        |> ProjectCreateForm.selectRecommendedOrFirstTemplate (getTags model) templates appState.config.template.recommendedTemplateUuid
                        |> ProjectCreateForm.selectRecommendedPackage (getTags model) templates
              }
            , Task.attempt (\_ -> cfg.wrapMsg NoOp) (Dom.focus "name")
            )

        Err error ->
            ( { model | templates = ApiError.toActionResult "Unable to get templates" error }
              --TODO maybe logout
            , Cmd.none
            )


handleGetKnowledgeModelComplete : Model -> Result ApiError KnowledgeModel -> ( Model, Cmd msg )
handleGetKnowledgeModelComplete model result =
    case result of
        Ok knowledgeModel ->
            ( { model | knowledgeModel = Success knowledgeModel }
            , Cmd.none
            )

        Err error ->
            ( { model | knowledgeModel = ApiError.toActionResult "Unable to get tags" error }
              -- TODO maybe logout
            , Cmd.none
            )


handlePostQuestionnaireComplete : UpdateConfig msg -> Model -> Result ApiError Questionnaire -> ( Model, Cmd msg )
handlePostQuestionnaireComplete cfg model result =
    case result of
        Ok questionnaire ->
            ( model, cfg.cmdNavigate (Route.Project questionnaire.uuid ProjectRoute.Overview) )

        Err error ->
            ( { model | submitting = ApiError.toActionResult "Unable to create project" error }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    { title = "Create project"
    , content = ActionResultWrapper.page appState.theme (viewContent appState model) model.templates
    }


viewContent : AppState -> Model -> List Template -> Html Msg
viewContent appState model templates =
    let
        grid =
            Grid.comfortable

        content =
            if model.screen == NameScreen then
                [ projectNameContainer appState model grid, emptyNode ]

            else
                [ emptyNode
                , projectSettingsCarouselContainer appState model templates grid
                , Html.map KnowledgeModelModalMsg <|
                    KnowledgeModelModal.view appState model.kmModalModel
                ]
    in
    div [] content


projectNameContainer : AppState -> Model -> Grid Msg -> Html Msg
projectNameContainer appState model grid =
    let
        projectNameFormGroup =
            Form.group
                { label = Form.labelBigger
                , input = FormInput.textWithAttrs [ autocomplete False ]
                , textBefore = Form.helpText
                , textAfter = Form.helpText
                , toMsg = FormMsg
                }
                { form = model.createForm
                , fieldName = "name"
                , mbFieldLabel = Just "Name your project"
                , mbTextBefore = Nothing
                , mbTextAfter = Just "Don't worry, you can always change it later."
                }
    in
    grid.container
        [ Grid.containerLimitedSmall
        , Grid.containerExtraIndented
        , Animation.fadeIn
        , Animation.fast
        ]
        [ grid.row []
            [ grid.col 5 [] [ Illustration.wizard appState.theme ]
            , grid.colOffset ( 1, 6 )
                [ Grid.colVerticalCenter ]
                [ projectNameFormGroup appState.theme
                , Button.primary appState.theme
                    [ onClick (SetScreen TemplateScreen)
                    , disabled (String.length (getProjectName model) == 0)
                    ]
                    [ span [] [ text "Get started" ]
                    , fa "fa-angle-right"
                    ]
                ]
            ]
        ]


projectSettingsCarouselContainer : AppState -> Model -> List Template -> Grid Msg -> Html Msg
projectSettingsCarouselContainer appState model templates grid =
    let
        ( progressCount, templatePage, kmPage ) =
            case model.screen of
                NameScreen ->
                    ( 1, Carousel.pageNext, Carousel.pageNext )

                TemplateScreen ->
                    ( 2, Carousel.pageCurrent, Carousel.pageNext )

                KnowledgeModelScreen ->
                    ( 3, Carousel.pagePrevious, Carousel.pageCurrent )
    in
    grid.container
        [ Grid.containerLimited, Grid.containerIndented, Animation.fadeIn, Animation.fast ]
        [ grid.row []
            [ grid.col 12 [] [ h1 [] [ text (getProjectName model) ] ] ]
        , grid.row []
            [ grid.col 12
                []
                [ ProgressBar.container appState.theme
                    progressCount
                    [ ProgressBar.item "far fa-folder-open" "Project name"
                    , ProgressBar.item "far fa-file-alt" "Document template"
                    , ProgressBar.item "fas fa-sitemap" "Knowledge model"
                    ]
                ]
            ]
        , grid.row [ Grid.rowStackLG ]
            [ grid.col 12
                []
                [ Carousel.container
                    [ projectSettingsCarouselTemplatePage appState model grid templates templatePage
                    , projectSettingsCarouselKnowledgeModelPage appState model grid kmPage
                    ]
                ]
            ]
        , projectSettingsButtons appState model grid
        ]


projectSettingsCarouselTemplatePage : AppState -> Model -> Grid Msg -> List Template -> PageOptions -> Html Msg
projectSettingsCarouselTemplatePage appState model grid templates pageOptions =
    let
        recommendedTemplateUuid =
            appState.config.template.recommendedTemplateUuid

        templateOptionsCount =
            if model.templateListExpanded then
                List.length templates

            else
                3

        templateOptions =
            templates
                |> List.sortWith (Template.compare recommendedTemplateUuid)
                |> List.take templateOptionsCount
                |> List.map (Template.toFormRichOption recommendedTemplateUuid)

        templateFormGroup =
            Form.groupSimple
                { input = FormInput.richRadioGroup templateOptions
                , toMsg = FormMsg
                }
                { form = model.createForm
                , fieldName = "templateUuid"
                , mbFieldLabel = Nothing
                , mbTextBefore = Nothing
                , mbTextAfter = Nothing
                }

        moreTemplatesLink =
            if not model.templateListExpanded && List.length templates > 3 then
                Button.link appState.theme
                    [ onClick (ExpandTemplateList True) ]
                    [ fa "fas fa-angle-down"
                    , span [] [ text "More templates" ]
                    ]

            else
                emptyNode

        kms =
            Maybe.unwrap [] .allowedPackages (getSelectedTemplate model)
                |> List.sortWith (TemplatePackage.compare (Maybe.map .recommendedPackageId (getSelectedTemplate model)))

        visibleKMs =
            if List.length kms > 4 then
                List.take 3 kms

            else
                kms

        emptyKMs =
            if List.isEmpty kms then
                Text.danger appState.theme "There are no knowledge models configured for the template."

            else
                emptyNode

        kmListItem km =
            li []
                [ Button.inline appState.theme
                    [ onClick (KnowledgeModelModalMsg (KnowledgeModelModal.open km.id)) ]
                    [ text km.name ]
                ]

        moreKMs =
            if List.length kms > 4 then
                li [ css [ listStyleType none ] ]
                    [ Text.lighter appState.theme ("And " ++ String.fromInt (List.length kms - 3) ++ " more") ]

            else
                emptyNode

        formats =
            getSelectedTemplate model
                |> Maybe.unwrap [] .formats
                |> List.map (\f -> ( f.shortName, f.color ))
    in
    Carousel.page pageOptions
        [ grid.block []
            [ grid.row []
                [ grid.col 12 [] [ h2 [] [ text "Choose a document template" ] ] ]
            , grid.row []
                [ grid.col 6
                    []
                    [ templateFormGroup appState.theme
                    , moreTemplatesLink
                    ]
                , grid.col 6
                    [ Grid.colSeparated appState.theme ]
                    [ h3 [] [ text "Available knowledge models" ]
                    , emptyKMs
                    , ul [] (List.map kmListItem visibleKMs ++ [ moreKMs ])
                    , h3 [] [ text "Supported formats" ]
                    , FileIconList.view formats
                    ]
                ]
            ]
        ]


projectSettingsCarouselKnowledgeModelPage : AppState -> Model -> Grid Msg -> PageOptions -> Html Msg
projectSettingsCarouselKnowledgeModelPage appState model grid pageOptions =
    let
        packages =
            Maybe.unwrap [] .allowedPackages (getSelectedTemplate model)

        recommendedPackage =
            Maybe.map .recommendedPackageId (getSelectedTemplate model)

        packageOptionsCount =
            if model.kmListExpanded then
                List.length packages

            else
                3

        kmOptions =
            packages
                |> List.sortWith (TemplatePackage.compare recommendedPackage)
                |> List.take packageOptionsCount
                |> List.map (TemplatePackage.toFormRichOption recommendedPackage)

        kmFormGroup =
            Form.groupSimple
                { input = FormInput.richRadioGroup kmOptions
                , toMsg = FormMsg
                }
                { form = model.createForm
                , fieldName = "packageId"
                , mbFieldLabel = Nothing
                , mbTextBefore = Nothing
                , mbTextAfter = Nothing
                }

        moreKMsLink =
            if not model.kmListExpanded && List.length packages > 3 then
                Button.link appState.theme
                    [ onClick (ExpandKMList True) ]
                    [ fa "fas fa-angle-down"
                    , span [] [ text "More knowledge models" ]
                    ]

            else
                emptyNode

        emptyKMs =
            if List.isEmpty packages then
                Text.danger appState.theme "There are no knowledge models configured for the selected template.\n\nTry to choose a different one."

            else
                emptyNode

        tagsFormGroup knowledgeModel =
            Form.groupSimple
                { input = FormInput.tagsGroup (KnowledgeModel.getTags knowledgeModel)
                , toMsg = FormMsg
                }
                { form = model.createForm
                , fieldName = "tagUuids"
                , mbFieldLabel = Just "Tags (advanced)"
                , mbTextBefore = Just "Use tags to filter the questions or select none to have all the questions available."
                , mbTextAfter = Nothing
                }

        viewTags knowledgeModel =
            if List.length knowledgeModel.tagUuids > 0 then
                div []
                    [ tagsFormGroup knowledgeModel appState.theme ]

            else
                emptyNode

        hasTags =
            ActionResult.unwrap False (not << List.isEmpty << .tagUuids) model.knowledgeModel

        tagViewAttributes =
            if hasTags then
                [ Grid.colSeparated appState.theme ]

            else
                []
    in
    Carousel.page pageOptions
        [ grid.block []
            [ grid.row []
                [ grid.col 12 [] [ h2 [] [ text "Choose a knowledge model" ] ] ]
            , grid.row []
                [ grid.col 6
                    []
                    [ kmFormGroup appState.theme
                    , moreKMsLink
                    , emptyKMs
                    ]
                , grid.col 6 tagViewAttributes [ ActionResultWrapper.blockSM appState.theme viewTags model.knowledgeModel ]
                ]
            ]
        ]


projectSettingsButtons : AppState -> Model -> Grid Msg -> Html Msg
projectSettingsButtons appState model grid =
    case model.screen of
        TemplateScreen ->
            projectSettingsCarouselTemplateButtons appState grid

        KnowledgeModelScreen ->
            projectSettingsCarouselKnowledgeModelButtons appState model grid

        _ ->
            emptyNode


projectSettingsCarouselTemplateButtons : AppState -> Grid Msg -> Html Msg
projectSettingsCarouselTemplateButtons appState grid =
    grid.row [ Grid.rowStackMD ]
        [ grid.col 6
            []
            [ Button.link appState.theme
                [ onClick (SetScreen NameScreen) ]
                [ fa "fa-angle-left"
                , span [] [ text "Back" ]
                ]
            ]
        , grid.col 6
            [ Grid.colTextRight ]
            [ Button.primary appState.theme
                [ onClick (SetScreen KnowledgeModelScreen) ]
                [ span [] [ text "Choose a knowledge model" ]
                , fa "fa-angle-right"
                ]
            ]
        ]


projectSettingsCarouselKnowledgeModelButtons : AppState -> Model -> Grid Msg -> Html Msg
projectSettingsCarouselKnowledgeModelButtons appState model grid =
    grid.row [ Grid.rowStackMD ]
        [ grid.col 6
            []
            [ Button.link appState.theme
                [ onClick (SetScreen TemplateScreen) ]
                [ fa "fa-angle-left"
                , span [] [ text "Back" ]
                ]
            ]
        , grid.col 6
            [ Grid.colTextRight ]
            [ Button.actionResultPrimary model.submitting
                appState.theme
                [ onClick (FormMsg <| Form.Submit) ]
                [ span [] [ text "Start planning" ]
                , fa "fa-angle-right"
                ]
            ]
        ]
