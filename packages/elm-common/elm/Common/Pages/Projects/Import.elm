module Common.Pages.Projects.Import exposing
    ( Model
    , Msg
    , Shown
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Importing replies into a project from a plugin.

**The plugin does the reading, this page does the deciding.** An importer is a
custom element the instance loads at start-up; it is given the knowledge model as
a string and hands back what it found (`PluginElement.onImport`). Nothing is sent
to the server on the way: what comes back is turned into questionnaire events
here (`Questionnaire.Importer`), shown against the project, and saved only when
somebody presses Import.

**What cannot be imported is shown beside what can.** A reply to a question the
knowledge model does not have, or of a kind that question does not take, is an
error rather than an event; both lists are on the page, and the one being read is
chosen with a switch rather than by scrolling past the other.

**The questionnaire is there to be read, not answered.** It is the project with
the imported replies already applied, read-only and without its toolbar, so what
the import will do can be seen in place. A change in the list leads to its
question in it.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.ProjectCommon as ProjectCommon
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.ProjectEvent.SetReplyData exposing (SetReplyData)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Projects as ProjectsApi
import Common.Components.DetailPage as DetailPage
import Common.Components.Markdown as Markdown
import Common.Components.Page as Page
import Common.Components.PluginView as PluginView
import Common.Components.Questionnaire as Questionnaire
import Common.Components.Questionnaire.Importer as Importer exposing (ImporterResult)
import Common.Components.UiWords as UiWords
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.ImporterEvent exposing (ImporterEvent)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Plugins.Plugin as Plugin
import Common.Plugins.PluginElement as PluginElement
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Flip exposing (flip)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, button, span, strong, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import String.Format as String
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Page.Workspace as Workspace
import Ui.Resizer as Resizer
import Ui.Tabs as Tabs
import Ui.TestId as TestId
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { uuid : Uuid
    , importerUrl : String
    , project : ActionResult ProjectQuestionnaire
    , questionnaire : ActionResult Questionnaire.Model

    -- The knowledge model as the plugin wants it: a string it parses itself.
    , knowledgeModelString : ActionResult String

    -- What came back from the plugin, once it has come back.
    , importResult : Maybe ImporterResult
    , shown : Shown
    , importing : ActionResult ()

    -- The column the changes are read in: as wide as somebody wants it, and on a
    -- narrow screen an overlay, as the questionnaire's chapters are.
    , changesResizer : Resizer.Model
    , changesOpen : Bool
    }


{-| Which of the two lists is being read.
-}
type Shown
    = Changes
    | Errors


changesResizerConfig : AppState -> Resizer.Config
changesResizerConfig appState =
    { edge = Resizer.Trailing
    , label = gettext "Resize the list of changes" appState.locale
    , min = 240
    , max = 640
    , default = 340
    , collapse = Nothing
    }


initialModel : AppState -> Uuid -> String -> Model
initialModel appState uuid importerUrl =
    { uuid = uuid
    , importerUrl = importerUrl
    , project = Loading
    , questionnaire = Loading
    , knowledgeModelString = Loading
    , importResult = Nothing
    , shown = Changes
    , importing = Unset
    , changesResizer = Resizer.init (changesResizerConfig appState)
    , changesOpen = False
    }


init : AppState -> Uuid -> String -> ( Model, Cmd Msg )
init appState uuid importerUrl =
    ( initialModel appState uuid importerUrl
    , ProjectsApi.getQuestionnaire appState uuid GotProject
    )



-- UPDATE


type Msg
    = GotProject (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | GotKnowledgeModelString (Result ApiError String)
    | GotImporterData (List ImporterEvent)
    | SetShown Shown
    | ChangesResizerMsg Resizer.Msg
    | SetChangesOpen Bool
    | SetChaptersOpen Bool
    | NoOp
    | Import
    | Imported (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, cmd )

        plain ( newModel, cmd ) =
            ( appState.seed, newModel, Cmd.map cfg.wrapMsg cmd )
    in
    case msg of
        GotProject result ->
            let
                setResult value m =
                    { m
                        | project = ActionResult.map .data value
                        , questionnaire = ActionResult.map (Tuple.first << Questionnaire.initSimple appState << .data) value
                    }

                ( withProject, resultCmd ) =
                    RequestHelpers.applyResultOrNotFound
                        { setResult = setResult
                        , defaultError = gettext "Unable to get the project." appState.locale
                        , notFoundCmd = UpdateConfig.notFound cfg (NotFound.project appState)
                        , notAllowedCmd = UpdateConfig.notAllowed cfg
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , locale = appState.locale
                        }

                -- The plugin needs the knowledge model the project runs on, with
                -- the tags it was narrowed to.
                knowledgeModelCmd =
                    case withProject.project of
                        Success project ->
                            Cmd.map cfg.wrapMsg
                                (KnowledgeModelsApi.fetchAsString appState
                                    project.knowledgeModelPackage.uuid
                                    project.selectedQuestionTagUuids
                                    GotKnowledgeModelString
                                )

                        _ ->
                            Cmd.none
            in
            ( appState.seed, withProject, Cmd.batch [ resultCmd, knowledgeModelCmd ] )

        GotKnowledgeModelString result ->
            withSeed <|
                RequestHelpers.applyResult
                    { setResult = \value m -> { m | knowledgeModelString = value }
                    , defaultError = gettext "Unable to get the knowledge model." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        GotImporterData events ->
            case ( model.project, model.questionnaire ) of
                ( Success project, Success questionnaire ) ->
                    let
                        ( seed, importResult ) =
                            Importer.convertToQuestionnaireEvents appState project (Ok events)
                    in
                    -- The questionnaire on screen is the project as it would be
                    -- after the import, so the events are applied to it here and
                    -- sent to the server only on Import.
                    ( seed
                    , { model
                        | importResult = Just importResult
                        , questionnaire = Success (List.foldl Questionnaire.applyProjectEvent questionnaire importResult.questionnaireEvents)
                        , shown =
                            if List.isEmpty importResult.questionnaireEvents && not (List.isEmpty importResult.errors) then
                                Errors

                            else
                                Changes
                      }
                    , Cmd.none
                    )

                _ ->
                    withSeed ( model, Cmd.none )

        SetShown shown ->
            -- The arrow keys move the selection, and only a command moves focus
            -- with it (`Ui.Tabs`).
            plain
                ( { model | shown = shown }
                , Tabs.focusTab NoOp changesTabsId (shownKey shown)
                )

        ChangesResizerMsg resizerMsg ->
            withSeed ( { model | changesResizer = Resizer.update resizerMsg model.changesResizer }, Cmd.none )

        SetChangesOpen open ->
            -- Two columns over the same narrow screen is one column too many:
            -- opening either closes the other.
            withChapters cfg appState False { model | changesOpen = open }

        SetChaptersOpen open ->
            withChapters cfg appState open { model | changesOpen = False }

        NoOp ->
            withSeed ( model, Cmd.none )

        Import ->
            case model.importResult of
                Just importResult ->
                    plain
                        ( { model | importing = Loading }
                        , ProjectsApi.putContent appState model.uuid importResult.questionnaireEvents Imported
                        )

                Nothing ->
                    withSeed ( model, Cmd.none )

        Imported result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Routes.navigate appState (Routes.projectsDetail model.uuid) )

                Err error ->
                    withSeed
                        ( { model | importing = ApiError.toActionResult appState (gettext "The changes could not be saved." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        QuestionnaireMsg questionnaireMsg ->
            withQuestionnaire cfg appState questionnaireMsg model


{-| One message through the questionnaire.
-}
withQuestionnaire : UpdateConfig Msg msg -> AppState -> Questionnaire.Msg -> Model -> ( Seed, Model, Cmd msg )
withQuestionnaire cfg appState questionnaireMsg model =
    case model.questionnaire of
        Success questionnaire ->
            let
                updated =
                    Questionnaire.update appState
                        { wrapMsg = cfg.wrapMsg << QuestionnaireMsg
                        , mbKmEditorUuid = Nothing
                        , projectCommon = ProjectCommon.dummy
                        , previewVersionMsg = Nothing
                        , revertVersionMsg = Nothing
                        }
                        questionnaireMsg
                        questionnaire
            in
            ( updated.seed, { model | questionnaire = Success updated.model }, updated.cmd )

        _ ->
            ( appState.seed, model, Cmd.none )


{-| The questionnaire's own column, opened or closed — which is the
questionnaire's to do, through the message it hands out
(`Questionnaire.navToggle`).
-}
withChapters : UpdateConfig Msg msg -> AppState -> Bool -> Model -> ( Seed, Model, Cmd msg )
withChapters cfg appState open model =
    case model.questionnaire of
        Success questionnaire ->
            withQuestionnaire cfg appState ((Questionnaire.navToggle questionnaire).onToggle open) model

        _ ->
            ( appState.seed, model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ChangesResizerMsg (Resizer.subscriptions model.changesResizer)
        , case model.questionnaire of
            Success questionnaire ->
                Sub.map QuestionnaireMsg (Questionnaire.subscriptions questionnaire)

            _ ->
                Sub.none
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState
        (viewImporter appState model)
        (ActionResult.combine3 model.project model.questionnaire model.knowledgeModelString)


viewImporter : AppState -> Model -> ( ProjectQuestionnaire, Questionnaire.Model, String ) -> Html Msg
viewImporter appState model ( project, questionnaire, knowledgeModelString ) =
    let
        -- The project is what is being imported into, so it is the page's
        -- name; what is being done to it is in the actions beside it.
        page content =
            DetailPage.detailPage { title = project.name, content = content }
    in
    DetailPage.toHtml <|
        case model.importResult of
            Just importResult ->
                page [ viewResult appState model project questionnaire importResult ]
                    -- The questionnaire beside the changes takes the whole of
                    -- the body, as the project's own tabs do.
                    |> DetailPage.withFlushContent True
                    |> DetailPage.withActions
                        [ Button.primary (gettext "Import" appState.locale) Import
                            |> Button.withLoading (ActionResult.isLoading model.importing)
                            |> Button.withDisabled (List.isEmpty importResult.questionnaireEvents)
                            |> Button.withTestId "project-import_import"
                        , Button.link (gettext "Cancel" appState.locale)
                            (Routes.toUrl appState (Routes.projectsDetail model.uuid))
                        ]

            Nothing ->
                page [ viewPlugin appState model project knowledgeModelString ]


{-| The importer itself, which is the plugin's own element. Where the instance
has no importer at that address there is nothing to draw, so the page says so
rather than showing an empty frame.
-}
viewPlugin : AppState -> Model -> ProjectQuestionnaire -> String -> Html Msg
viewPlugin appState model project knowledgeModelString =
    let
        importer =
            AppState.getPluginsByConnector appState .projectImporters
                |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId project.knowledgeModelPackage)
                |> List.find (\( _, connector ) -> connector.url == model.importerUrl)
    in
    case importer of
        Just ( plugin, connector ) ->
            -- On the page's body, padded as every page is, and nothing more
            -- of ours round it.
            PluginView.view appState
                plugin.uuid
                connector.element
                [ PluginElement.knowledgeModelValue knowledgeModelString
                , PluginElement.projectValue (ProjectQuestionnaire.toProjectCommon project)
                , PluginElement.onImport GotImporterData
                ]

        Nothing ->
            Page.errorWithinPage appState (gettext "This importer is not available." appState.locale)


{-| What the import would do: the changes in a column of their own, the project
as it would be beside them.

A workspace, so the column is the reader's to widen and, on a narrow screen,
opens over the questionnaire from a button rather than taking half the height of
it — the same column the chapters are in, behaving the same way.

-}
viewResult : AppState -> Model -> ProjectQuestionnaire -> Questionnaire.Model -> ImporterResult -> Html Msg
viewResult appState model project questionnaire importResult =
    Workspace.toHtml
        (Workspace.workspace
            { id = workspaceId
            , main =
                [ Questionnaire.view appState
                    { wrapMsg = QuestionnaireMsg
                    , readonly = True
                    , commentsReadonly = True
                    , actionsEnabled = False
                    , toolbarEnabled = False
                    , canViewVersions = False
                    , canRevert = False
                    }
                    questionnaire
                ]
            }
            |> Workspace.withFlushMain
            -- Both columns fold away on a narrow screen, and the control for the
            -- questionnaire's own belongs in the row that already holds ours
            -- rather than in a second row under it.
            |> Workspace.withNavToggleExtras [ viewChaptersToggle appState questionnaire ]
            |> Workspace.withNav
                { label = gettext "Changes to import" appState.locale

                -- What is being imported and which of the two lists is being
                -- read stay put; the list under them is what scrolls.
                , header = viewChangesHeader appState model project importResult
                , content = viewChangesList appState model project importResult
                , resizer = model.changesResizer
                , toResizerMsg = ChangesResizerMsg
                , resizerWords = UiWords.resizer appState (gettext "Resize the list of changes" appState.locale)
                , overlayOpen = model.changesOpen
                , onOverlay = SetChangesOpen
                , toggleLabel = gettext "Changes" appState.locale
                , closeLabel = gettext "Close the changes" appState.locale
                }
        )


{-| The questionnaire's own column, opened from our row.

Drawn here rather than by the questionnaire, which draws it only with the toolbar
this page does not show; the questionnaire says what the control needs to know
(`Questionnaire.navToggle`).

-}
viewChaptersToggle : AppState -> Questionnaire.Model -> Html Msg
viewChaptersToggle appState questionnaire =
    let
        chapters =
            Questionnaire.navToggle questionnaire
    in
    button
        [ type_ "button"
        , class "ui-button ui-button--ghost ui-workspace__nav-toggle"
        , attribute "aria-expanded"
            (if chapters.open then
                "true"

             else
                "false"
            )
        , attribute "aria-controls" chapters.controls
        , onClick (SetChaptersOpen (not chapters.open))
        ]
        [ Icon.toHtml (Icon.solid "fa-list-ul")
        , span [] [ text (gettext "Chapters" appState.locale) ]
        ]


workspaceId : String
workspaceId =
    "project-import"


changesTabsId : String
changesTabsId =
    "project-import-changes"


shownKey : Shown -> String
shownKey shown =
    case shown of
        Changes ->
            "changes"

        Errors ->
            "errors"


{-| How much is about to change, and which of the two lists is being read. Both
stay while the list scrolls, so what is being decided is never scrolled away
from.
-}
viewChangesHeader : AppState -> Model -> ProjectQuestionnaire -> ImporterResult -> List (Html Msg)
viewChangesHeader appState model project importResult =
    let
        changeCount =
            List.length importResult.questionnaireEvents

        errorCount =
            List.length importResult.errors

        summary =
            Text.bodyHtml
                (String.formatHtml
                    (ngettext ( "%s change will be imported into %s.", "%s changes will be imported into %s." ) changeCount appState.locale)
                    [ strong [] [ text (String.fromInt changeCount) ]
                    , text project.name
                    ]
                )

        count toBadge number =
            Badge.toHtml (toBadge (String.fromInt number))

        -- Two lists, one at a time, because both can be long and the one being
        -- read is a choice rather than a scroll. Only where there is a second
        -- one to choose.
        tabs =
            if errorCount == 0 then
                []

            else
                [ Tabs.toHtmlWithTabIds shownKey
                    { id = changesTabsId
                    , label = gettext "What to look at" appState.locale
                    , active = model.shown
                    , full = True
                    , tabs =
                        [ Tabs.tab { id = Changes, label = gettext "Changes" appState.locale, onSelect = SetShown Changes }
                            |> Tabs.withBadge (count Badge.neutral changeCount)

                        -- What could not be read is a problem, and the count
                        -- says so where it is counted.
                        , Tabs.tab { id = Errors, label = gettext "Errors" appState.locale, onSelect = SetShown Errors }
                            |> Tabs.withBadge (count Badge.danger errorCount)
                        ]
                    }
                ]
    in
    summary
        :: tabs
        ++ (case model.importing of
                Error message ->
                    [ Alert.toHtml (Alert.danger message) ]

                _ ->
                    []
           )


{-| The list being read: what will be imported, or what could not be.
-}
viewChangesList : AppState -> Model -> ProjectQuestionnaire -> ImporterResult -> List (Html Msg)
viewChangesList appState model project importResult =
    let
        body =
            case model.shown of
                Changes ->
                    if List.isEmpty importResult.questionnaireEvents then
                        [ Alert.toHtml (Alert.warning (gettext "There is nothing to import." appState.locale)) ]

                    else
                        List.filterMap (viewChange appState project) importResult.questionnaireEvents

                Errors ->
                    List.map (Alert.toHtml << Alert.danger) importResult.errors
    in
    [ Tabs.panel
        { id = changesTabsId
        , label =
            case model.shown of
                Changes ->
                    gettext "Changes" appState.locale

                Errors ->
                    gettext "Errors" appState.locale
        }
        [ Layout.stack body ]
    ]


{-| One change: the question it answers, and the answer it gives. The question
is a link into the questionnaire beside it, so a change can be read where it
lands.
-}
viewChange : AppState -> ProjectQuestionnaire -> ProjectEvent -> Maybe (Html Msg)
viewChange appState project event =
    let
        question =
            ProjectEvent.getQuestionUuid event
                |> Maybe.andThen (flip KnowledgeModel.getQuestion project.knowledgeModel)
    in
    case ( event, question ) of
        ( ProjectEvent.SetReply data, Just found ) ->
            Just (viewReply appState project found data)

        _ ->
            Nothing


viewReply : AppState -> ProjectQuestionnaire -> Question -> SetReplyData -> Html Msg
viewReply appState project question data =
    let
        -- An item added to a list is read at the item rather than at the list.
        path =
            case data.value of
                ReplyValue.ItemListReply items ->
                    Maybe.unwrap data.path (\item -> data.path ++ "." ++ item) (List.last items)

                _ ->
                    data.path

        said words =
            [ text words ]

        replies =
            case data.value of
                ReplyValue.StringReply reply ->
                    [ ( Icon.regular "fa-pen-to-square", said reply ) ]

                ReplyValue.AnswerReply answerUuid ->
                    [ ( Icon.regular "fa-circle-dot"
                      , said (Maybe.unwrap "" .label (KnowledgeModel.getAnswer answerUuid project.knowledgeModel))
                      )
                    ]

                ReplyValue.MultiChoiceReply choiceUuids ->
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) project.knowledgeModel
                        |> List.filter (.uuid >> flip List.member choiceUuids)
                        |> List.map (\choice -> ( Icon.regular "fa-square-check", said choice.label ))

                ReplyValue.ItemListReply _ ->
                    [ ( Icon.solid "fa-plus", said (gettext "Added item" appState.locale) ) ]

                ReplyValue.ItemSelectReply _ ->
                    [ ( Icon.solid "fa-plus", said (gettext "Added item" appState.locale) ) ]

                ReplyValue.FileReply _ ->
                    [ ( Icon.solid "fa-plus", said (gettext "Added file" appState.locale) ) ]

                ReplyValue.IntegrationReply replyType ->
                    case replyType of
                        IntegrationReplyType.PlainType reply ->
                            [ ( Icon.regular "fa-pen-to-square", said reply ) ]

                        -- What an integration answers with is markdown, as it
                        -- is wherever else a reply from one is shown.
                        IntegrationReplyType.IntegrationType reply _ ->
                            [ ( Icon.solid "fa-link", Markdown.inline reply ) ]

        replyItem ( icon, reply ) =
            span [ class "c-project-import__reply" ]
                (Icon.toHtml icon :: reply)
    in
    -- A card each, as the wizard had them: one change is one thing to read, and a
    -- column of them reads as a list of changes rather than as a page of text.
    --
    -- The card is the control, not something with a control in it: all of it says
    -- the same thing and leads to the same place, and a card-sized target is
    -- easier to hit than the words in it (2.5.8). So it is a `button`, and what
    -- is in it is phrasing content — spans rather than a list — because that is
    -- all a button may hold.
    button
        [ type_ "button"
        , class "ui-card c-project-import__change"
        , onClick (QuestionnaireMsg (Questionnaire.scrollToPathMsg False path))
        , TestId.attribute "project-import_change"
        ]
        (span [ class "c-project-import__question" ] [ text (Question.getTitle question) ]
            :: List.map replyItem replies
        )
