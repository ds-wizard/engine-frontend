module Common.Pages.Documents.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The documents listing, of every project or of one (`?projectUuid`).

Documents are generated in the background, so while any document on the page is
queued or in progress the page asks again every second, without the loading
state and without closing an open menu (`Listing.reloadBackground`).

A document that is done can be downloaded (in place, as in the wizard) and, where
submission is on, submitted to a service. One that failed can show why. Plugins
may add their own actions.

When the listing is of one project, that project is a filter in the toolbar: it
says which project, links to it, and can be cleared.

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
import Common.Components.GuideLink as GuideLink
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Page as Page
import Common.Components.PluginModal as PluginModal
import Common.Components.SubmitDocumentModal as SubmitDocumentModal
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
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
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.CodeBlock as CodeBlock
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Menu as Menu
import Ui.Modal as Modal
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { documents : Listing.Model Document
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult ()
    , projectUuid : Maybe Uuid
    , project : Maybe (ActionResult ProjectCommon)
    , submitModal : SubmitDocumentModal.Model
    , documentErrorModal : Maybe String
    , submissionErrorModal : Maybe String
    , pluginModal : PluginModal.Model Document
    , rowActionsOrigin : Maybe Uuid
    }


initialModel : Maybe Uuid -> PaginationQueryString -> Model
initialModel projectUuid pqs =
    { documents = Listing.initialModel pqs
    , documentToBeDeleted = Nothing
    , deletingDocument = Unset
    , projectUuid = projectUuid
    , project = Maybe.map (always Loading) projectUuid
    , submitModal = SubmitDocumentModal.initialModel
    , documentErrorModal = Nothing
    , submissionErrorModal = Nothing
    , pluginModal = PluginModal.initialModel
    , rowActionsOrigin = Nothing
    }


init : AppState -> Maybe Uuid -> PaginationQueryString -> ( Model, Cmd Msg )
init appState projectUuid pqs =
    let
        ( documents, listingCmd ) =
            Listing.init pqs PaginationQueryFilters.empty

        initialModelValue =
            initialModel projectUuid pqs

        projectCmd =
            case projectUuid of
                Just uuid ->
                    ProjectsApi.get appState uuid GetProjectCompleted

                Nothing ->
                    Cmd.none
    in
    ( { initialModelValue | documents = documents }
    , Cmd.batch [ Cmd.map ListingMsg listingCmd, projectCmd ]
    )


anyDocumentInProgress : Model -> Bool
anyDocumentInProgress model =
    Listing.items model.documents
        |> List.any (\document -> document.state == QueuedDocumentState || document.state == InProgressDocumentState)



-- UPDATE


type Msg
    = GetProjectCompleted (Result ApiError ProjectCommon)
    | ClearProjectFilter
    | ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Document)
    | SubmitModalMsg SubmitDocumentModal.Msg
    | DocumentSubmitted Submission
    | SetDocumentErrorModal (Maybe String)
    | SetSubmissionErrorModal (Maybe String)
    | DownloadDocument Document
    | GotDocumentUrlCompleted (Result ApiError UrlResponse)
    | PluginModalMsg (PluginModal.Msg Document)
    | RowAction Uuid Msg
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetProjectCompleted result ->
            RequestHelpers.applyResultOrNotFound
                { setResult = \value record -> { record | project = Just value }
                , defaultError = gettext "Unable to get documents." appState.locale
                , notFoundCmd = UpdateConfig.notFound cfg (NotFound.project appState)
                , notAllowedCmd = UpdateConfig.notAllowed cfg
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ClearProjectFilter ->
            let
                newModel =
                    { model | projectUuid = Nothing, project = Nothing }

                ( documents, cmd ) =
                    Listing.update appState
                        (listingUpdateConfig cfg appState newModel)
                        (Listing.setFilters (Listing.filters model.documents))
                        model.documents
            in
            -- The filter, and the menu item that had focus, are gone.
            ( { newModel | documents = documents }
            , Cmd.batch [ cmd, AppShell.focusOnRouteChange (cfg.wrapMsg NoOp) ]
            )

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

        ListingMsg listingMsg ->
            let
                ( documents, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState model) listingMsg model.documents
            in
            ( { model | documents = documents }, cmd )

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
                    Listing.update appState (listingUpdateConfig cfg appState model) Listing.closeMenus model.documents
            in
            update cfg appState rowMsg { model | documents = documents, rowActionsOrigin = Just documentUuid }

        NoOp ->
            ( model, Cmd.none )


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


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Model -> Listing.UpdateConfig Document msg
listingUpdateConfig cfg appState model =
    { getRequest = DocumentsApi.getDocuments appState model.projectUuid
    , getError = gettext "Unable to get documents." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.documentsIndexWithFilters model.projectUuid filters pqs)
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


view : AppState -> Model -> Html Msg
view appState model =
    case model.project of
        Just projectResult ->
            Page.actionResult appState (viewDocuments appState model << Just) projectResult

        Nothing ->
            viewDocuments appState model Nothing


viewDocuments : AppState -> Model -> Maybe ProjectCommon -> Html Msg
viewDocuments appState model mbProject =
    Listing.viewWith appState
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
        (listingConfig appState mbProject)
        model.documents


listingConfig : AppState -> Maybe ProjectCommon -> Listing.ViewConfig Document Msg
listingConfig appState mbProject =
    let
        projectFilter project =
            Listing.CustomFilter "projectUuid"
                { name = gettext "Project" appState.locale
                , value = Just project.name
                , active = True
                , items =
                    [ Menu.withIcon (Icon.regular "fa-folder-open")
                        (Menu.linkItem (gettext "Open project" appState.locale) (Routes.toUrl appState (Routes.projectsDetail project.uuid)))
                    , Menu.separator
                    , Menu.item (gettext "Clear selection" appState.locale) ClearProjectFilter
                    ]
                }
    in
    { title = gettext "Project Documents" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsDocuments ]
    , searchPlaceholder = gettext "Search documents..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = Maybe.unwrap [] (List.singleton << projectFilter) mbProject
    , item = listingItem appState
    , emptyTitle = gettext "No documents" appState.locale
    , emptyDescription = gettext "There are no documents." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> Document -> Listing.Item Msg
listingItem appState document =
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

        projectFragment =
            Maybe.map
                (\project ->
                    Link.toHtml
                        (Link.link
                            { label = project.name
                            , url = Routes.toUrl appState (Routes.projectsDetail project.uuid)
                            }
                        )
                )
                document.project

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
                , projectFragment
                , documentTemplateFragment
                ]
        , detail = submissionsDetail
        , timestamp = Just (Timestamp.created appState document.createdAt)
        , actions = listingActions appState document
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


listingActions : AppState -> Document -> List (ItemActions.Action Msg)
listingActions appState document =
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
          , ( submit, Feature.documentSubmit appState document )
          , ( viewError, Maybe.isJust document.workerLog && document.state == ErrorDocumentState )
          ]
        , DocumentPluginActions.actions appState document (RowAction document.uuid << PluginModalMsg)
        , [ ( delete, Feature.documentDelete appState document ) ]
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
        , testId = Just "document-delete"
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
