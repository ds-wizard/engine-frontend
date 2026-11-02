module Common.Pages.Projects.Detail.Documents exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The documents tab of one project: everything generated from it, and what can
be done with each one.

The same list as `Common.Pages.Documents.Index` narrowed to this project, so it is
built the same way and out of the same parts — the row actions, the submit
dialog and the plugin dialog are shared. What is different is where it sits: it
is a region of the project's page rather than a page, so it has no `h1` of its
own (`Listing.viewWithinPageWith`) and it is the project, not a filter, that
says which documents these are.

Documents are generated in the background, so while any document on the page is
queued or in progress the list is asked for again every second, without the
loading state and without closing an open menu (`Listing.reloadBackground`).

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Documents as DocumentsApi
import Common.Api.Models.Document exposing (Document)
import Common.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Common.Api.Models.ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.Submission as Submission exposing (Submission)
import Common.Api.Models.Submission.SubmissionState as SubmissionState
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DocumentPluginActions as DocumentPluginActions
import Common.Components.EntityCard as EntityCard
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.PluginModal as PluginModal
import Common.Components.SubmitDocumentModal as SubmitDocumentModal
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.ProjectUtils as ProjectUtils
import Common.Data.Session as Session
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Plugins.PluginElement as PluginElement
import Common.Ports.File as File
import Common.Routes as Routes
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Time
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.CodeBlock as CodeBlock
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Modal as Modal
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { documents : Listing.Model Document
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult ()
    , submitModal : SubmitDocumentModal.Model
    , documentErrorModal : Maybe String
    , submissionErrorModal : Maybe String
    , pluginModal : PluginModal.Model Document
    , rowActionsOrigin : Maybe Uuid
    }


initialModel : Model
initialModel =
    { documents = Listing.initialModel PaginationQueryString.empty
    , documentToBeDeleted = Nothing
    , deletingDocument = Unset
    , submitModal = SubmitDocumentModal.initialModel
    , documentErrorModal = Nothing
    , submissionErrorModal = Nothing
    , pluginModal = PluginModal.initialModel
    , rowActionsOrigin = Nothing
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init pqs =
    let
        ( documents, cmd ) =
            Listing.init pqs PaginationQueryFilters.empty
    in
    ( { initialModel | documents = documents }, Cmd.map ListingMsg cmd )


anyDocumentInProgress : Model -> Bool
anyDocumentInProgress model =
    Listing.items model.documents
        |> List.any (\document -> document.state == QueuedDocumentState || document.state == InProgressDocumentState)



-- UPDATE


type Msg
    = ListingMsg (Listing.Msg Document)
    | ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | SubmitModalMsg SubmitDocumentModal.Msg
    | DocumentSubmitted Submission
    | SetDocumentErrorModal (Maybe String)
    | SetSubmissionErrorModal (Maybe String)
    | DownloadDocument Document
    | GotDocumentUrlCompleted (Result ApiError UrlResponse)
    | PluginModalMsg (PluginModal.Msg Document)
    | RowAction Uuid Msg
    | ViewQuestionnaire Uuid String
    | NoOp


{-| What the page around the documents does for them. Showing the questionnaire a
document was made from is the project's dialog, not this tab's: the same one its
version history opens (`Common.Pages.Projects.Components.VersionViewModal`).
-}
type alias PageConfig msg =
    { onViewQuestionnaire : Uuid -> String -> msg }


update : UpdateConfig Msg msg -> PageConfig msg -> AppState -> Uuid -> Msg -> Model -> ( Model, Cmd msg )
update cfg page appState projectUuid msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( documents, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState projectUuid) listingMsg model.documents
            in
            ( { model | documents = documents }, cmd )

        ShowHideDeleteDocument mbDocument ->
            ( { model | documentToBeDeleted = mbDocument, deletingDocument = Unset }
            , case ( mbDocument, model.documentToBeDeleted ) of
                ( Nothing, Just document ) ->
                    focusRowActions cfg document.uuid

                _ ->
                    Cmd.none
            )

        DeleteDocument ->
            case model.documentToBeDeleted of
                Just document ->
                    ( { model | deletingDocument = Loading }
                    , DocumentsApi.deleteDocument appState (Uuid.toString document.uuid) (cfg.wrapMsg << DeleteDocumentCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteDocumentCompleted result ->
            case result of
                Ok _ ->
                    ( { model | documentToBeDeleted = Nothing, deletingDocument = Unset }
                    , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    )

                Err error ->
                    ( { model | deletingDocument = ApiError.toActionResult appState (gettext "Document could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SubmitModalMsg modalMsg ->
            let
                ( submitModal, cmd ) =
                    SubmitDocumentModal.update
                        { wrapMsg = cfg.wrapMsg << SubmitModalMsg
                        , logoutMsg = cfg.logoutMsg
                        , submittedMsg = cfg.wrapMsg << DocumentSubmitted
                        , closeCmd = focusRowActions cfg
                        }
                        appState
                        modalMsg
                        model.submitModal
            in
            ( { model | submitModal = submitModal }, cmd )

        DocumentSubmitted submission ->
            let
                addSubmission document =
                    if submission.documentUuid == document.uuid then
                        { document | submissions = submission :: document.submissions }

                    else
                        document
            in
            ( { model | documents = Listing.updateItems addSubmission model.documents }, Cmd.none )

        SetDocumentErrorModal mbError ->
            ( { model | documentErrorModal = mbError }
            , if Maybe.isNothing mbError && Maybe.isJust model.documentErrorModal then
                focusRowActionsOrigin cfg model

              else
                Cmd.none
            )

        SetSubmissionErrorModal mbError ->
            ( { model | submissionErrorModal = mbError }, Cmd.none )

        DownloadDocument document ->
            ( model, DocumentsApi.getDocumentUrl appState document.uuid (cfg.wrapMsg << GotDocumentUrlCompleted) )

        GotDocumentUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( model, File.downloadFile urlResponse.url )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        PluginModalMsg pluginModalMsg ->
            let
                pluginModal =
                    PluginModal.update pluginModalMsg model.pluginModal
            in
            ( { model | pluginModal = pluginModal }
            , if Maybe.isNothing pluginModal.state && Maybe.isJust model.pluginModal.state then
                focusRowActionsOrigin cfg model

              else
                Cmd.none
            )

        RowAction documentUuid rowMsg ->
            let
                ( documents, _ ) =
                    Listing.update appState (listingUpdateConfig cfg appState projectUuid) Listing.closeMenus model.documents
            in
            update cfg page appState projectUuid rowMsg { model | documents = documents, rowActionsOrigin = Just documentUuid }

        ViewQuestionnaire eventUuid title ->
            ( model, dispatch (page.onViewQuestionnaire eventUuid title) )

        NoOp ->
            ( model, Cmd.none )


{-| A message for the page around this one, sent as a command because that is the
only way out of an update.
-}
dispatch : msg -> Cmd msg
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


{-| A dialog a row's action opened was dismissed and the row is still there:
focus goes back to the row's menu, which the dialog had made inert. A task, so
it runs once the dialog has closed.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


{-| `focusRowActions` for a dialog that does not know its document — the error
log and a plugin's — back to the row whose menu was used last.
-}
focusRowActionsOrigin : UpdateConfig Msg msg -> Model -> Cmd msg
focusRowActionsOrigin cfg model =
    Maybe.unwrap Cmd.none (focusRowActions cfg) model.rowActionsOrigin


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Uuid -> Listing.UpdateConfig Document msg
listingUpdateConfig cfg appState projectUuid =
    { getRequest = ProjectsApi.getDocuments appState projectUuid
    , getError = gettext "Unable to get documents." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.projectsDetailDocumentsWithFilters projectUuid filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if anyDocumentInProgress model then
            Time.every 1000 (\_ -> ListingMsg Listing.reloadBackground)

          else
            Sub.none
        , Sub.map ListingMsg (Listing.subscriptions model.documents)
        ]



-- VIEW


view : AppState -> ProjectCommon -> Model -> Html Msg
view appState project model =
    Listing.viewWithinPageWith appState
        { notices = []
        , overlays =
            [ deleteModal appState model
            , Html.map SubmitModalMsg (SubmitDocumentModal.view appState model.submitModal)
            , errorModal appState
                { title = gettext "Document error" appState.locale
                , testId = "document-error"
                , error = model.documentErrorModal
                , onClose = SetDocumentErrorModal Nothing
                }
            , errorModal appState
                { title = gettext "Submission error" appState.locale
                , testId = "submission-error"
                , error = model.submissionErrorModal
                , onClose = SetSubmissionErrorModal Nothing
                }
            , PluginModal.view appState
                { attributes = \document -> [ PluginElement.documentValue document ]
                , wrapMsg = PluginModalMsg
                }
                model.pluginModal
            ]
        }
        (listingConfig appState project)
        model.documents


{-| Creating a document is something only an editor who is signed in can do, so
that is who is offered it — and who the empty list tells how to fill it.
-}
canCreate : AppState -> ProjectCommon -> Bool
canCreate appState project =
    ProjectUtils.isEditor appState project && Session.exists appState.session


listingConfig : AppState -> ProjectCommon -> Listing.ViewConfig Document Msg
listingConfig appState project =
    { title = gettext "Documents" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions =
        if canCreate appState project then
            [ Button.linkPrimary (gettext "New document" appState.locale)
                (Routes.toUrl appState (Routes.projectsDetailDocumentsNew project.uuid Nothing))
                |> Button.withIcon (Icon.solid "fa-plus")
                |> Button.withTestId "project-documents_new-document-button"
            ]

        else
            []
    , searchPlaceholder = gettext "Search documents..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , item = listingItem appState project
    , emptyTitle = gettext "No documents" appState.locale
    , emptyDescription =
        if canCreate appState project then
            gettext "Click \"New document\" button to add a new document." appState.locale

        else if Session.exists appState.session then
            gettext "There are no documents." appState.locale

        else
            gettext "Log in to add a new document." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> ProjectCommon -> Document -> Listing.Item Msg
listingItem appState project document =
    let
        row =
            Listing.item
                { id = Uuid.toString document.uuid
                , title = documentTitle appState document
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ document.name ]
                }

        formatFragment =
            Just
                (span []
                    [ Icon.toHtml (Icon.fromClass document.format.icon)
                    , text " "
                    , text document.format.name
                    ]
                )

        languageFragment =
            Maybe.map
                (\language ->
                    span []
                        [ Icon.toHtml (Icon.solid "fa-language")
                        , text " "
                        , text language
                        ]
                )
                document.language

        fileSizeFragment =
            Maybe.map (text << ByteUnits.toReadable) document.fileSize

        documentTemplateFragment =
            Just
                (span []
                    [ Link.toHtml
                        (Link.link
                            { label = document.documentTemplate.name
                            , url = Routes.toUrl appState (Routes.documentTemplatesDetail document.documentTemplate.uuid)
                            }
                        )
                    , text " "

                    -- The link already names the template, so the badge gives
                    -- what the link cannot: which version of it made this.
                    , Badge.toHtml (Badge.neutral (Version.toString document.documentTemplate.version))
                    ]
                )

        -- Which version of the project this was generated from, when it was not
        -- the current one.
        versionFragment =
            Maybe.map (Badge.toHtml << Badge.neutral) document.projectVersion

        submissionsDetail =
            case List.sortWith Submission.compare document.submissions of
                [] ->
                    Nothing

                submissions ->
                    Just (Layout.stack (List.map (viewSubmission appState) submissions))
    in
    { row
        | icon = Just (ItemIcon.toHtml (UserAvatar.item document.name Nothing))
        , badges = Maybe.values [ stateBadge appState document.state ]
        , fragments =
            Maybe.values
                [ formatFragment
                , languageFragment
                , fileSizeFragment
                , documentTemplateFragment
                , versionFragment
                ]
        , detail = submissionsDetail
        , timestamp = Just (Timestamp.created appState document.createdAt)
        , actions = listingActions appState project document
    }


{-| A done document's name downloads it, as its Download action does; one not
yet done, or failed, has nothing to download, so its name is only text.
-}
documentTitle : AppState -> Document -> Html Msg
documentTitle appState document =
    if Feature.documentDownload document then
        Link.actionWithDescription "listing-item_title-download"
            { id = "document-title-" ++ Uuid.toString document.uuid
            , label = document.name
            , onClick = DownloadDocument document
            , description = gettext "Click to download" appState.locale
            }

    else
        text document.name


listingActions : AppState -> ProjectCommon -> Document -> List (ItemActions.Action Msg)
listingActions appState project document =
    let
        download =
            ItemActions.action
                { icon = Icon.solid "fa-download"
                , label = gettext "Download" appState.locale
                , onSelect = RowAction document.uuid (DownloadDocument document)
                }
                |> ItemActions.withTestId "listing-item_action_download"

        submit =
            ItemActions.action
                { icon = Icon.solid "fa-paper-plane"
                , label = gettext "Submit" appState.locale
                , onSelect = RowAction document.uuid (SubmitModalMsg (SubmitDocumentModal.open document))
                }
                |> ItemActions.withTestId "listing-item_action_submit"

        viewError =
            ItemActions.action
                { icon = Icon.solid "fa-circle-exclamation"
                , label = gettext "View error" appState.locale
                , onSelect = RowAction document.uuid (SetDocumentErrorModal document.workerLog)
                }
                |> ItemActions.withTestId "listing-item_action_view-error"

        -- The questionnaire as it was when this document was made, in the same
        -- dialog the version history opens. Only where the document says which
        -- version it came from; older documents do not.
        viewQuestionnaire eventUuid =
            ItemActions.action
                { icon = Icon.regular "fa-rectangle-list"
                , label = gettext "View questionnaire" appState.locale
                , onSelect = RowAction document.uuid (ViewQuestionnaire eventUuid document.name)
                }
                |> ItemActions.withTestId "listing-item_action_view-questionnaire"

        delete =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = RowAction document.uuid (ShowHideDeleteDocument (Just document))
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    ItemActions.groups
        [ [ ( download, Feature.documentDownload document )
          , ( submit, Feature.projectDocumentSubmit appState project document )
          ]
        , [ ( viewError, Maybe.isJust document.workerLog && document.state == ErrorDocumentState ) ]
        , case document.projectEventUuid of
            Just eventUuid ->
                [ ( viewQuestionnaire eventUuid, True ) ]

            Nothing ->
                []
        , DocumentPluginActions.actions appState document (RowAction document.uuid << PluginModalMsg)
        , [ ( delete, canCreate appState project ) ]
        ]


stateBadge : AppState -> DocumentState -> Maybe (Badge.Badge msg)
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            Just (Badge.info (gettext "Queued" appState.locale))

        InProgressDocumentState ->
            Just (Badge.info (gettext "In Progress" appState.locale))

        DoneDocumentState ->
            Nothing

        ErrorDocumentState ->
            Just (Badge.danger (gettext "Error" appState.locale))


{-| A submission of the document: to which service, how it went, a way to see
the result, and when.
-}
viewSubmission : AppState -> Submission -> Html Msg
viewSubmission appState submission =
    let
        stateBadge_ =
            case submission.state of
                SubmissionState.InProgress ->
                    Badge.info (gettext "Submitting" appState.locale)

                SubmissionState.Done ->
                    Badge.success (gettext "Submitted" appState.locale)

                SubmissionState.Error ->
                    Badge.danger (gettext "Error" appState.locale)

        link =
            case ( submission.state, submission.location, submission.returnedData ) of
                ( SubmissionState.Done, Just location, _ ) ->
                    [ text " "
                    , Link.link { label = gettext "View submission" appState.locale, url = location }
                        |> Link.external
                        |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                        |> Link.toHtml
                    ]

                ( SubmissionState.Error, _, Just _ ) ->
                    [ text " "
                    , Button.ghost (gettext "View error" appState.locale) (SetSubmissionErrorModal (Just (Submission.getReturnedData submission)))
                        |> Button.small
                        |> Button.toHtml
                    ]

                _ ->
                    []
    in
    span []
        ([ text (Submission.visibleName submission)
         , text " "
         , Badge.toHtml stateBadge_
         , text " "
         , Timestamp.view appState submission.updatedAt
         ]
            ++ link
        )


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete document" appState.locale
        , open = Maybe.isJust model.documentToBeDeleted
        , question = gettext "Are you sure you want to permanently delete this document?" appState.locale
        , entity =
            EntityCard.slot
                (Maybe.map
                    (\document ->
                        { icon = UserAvatar.item document.name Nothing
                        , name = document.name
                        , badges = []
                        , code = Nothing
                        , detail = Nothing
                        }
                    )
                    model.documentToBeDeleted
                )
        , onConfirm = DeleteDocument
        , onClose = ShowHideDeleteDocument Nothing
        , result = model.deletingDocument
        , testId = Just "documents-delete"
        }


errorModal : AppState -> { title : String, testId : String, error : Maybe String, onClose : Msg } -> Html Msg
errorModal appState config =
    Modal.plain
        { title = config.title
        , open = Maybe.isJust config.error
        , onClose = config.onClose
        , body =
            [ div []
                [ CodeBlock.toHtml
                    { label = config.title
                    , language = "text"
                    , value = Maybe.withDefault "" config.error
                    }
                ]
            ]
        }
        |> Modal.withWide
        |> Modal.withCancelLabel (gettext "OK" appState.locale)
        |> Modal.withTestId config.testId
        |> Modal.toHtml
