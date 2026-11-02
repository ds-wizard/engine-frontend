module Common.Pages.ProjectFiles.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Every file uploaded to a project's questionnaire, across projects: its size,
its project, who uploaded it, a download and a delete.

The download happens in place, as in the wizard: the server is asked for the
file's URL and the browser is handed it, so the listing stays where it was.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectFile exposing (ProjectFile)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.Models.User as User
import Common.Api.ProjectFiles as ProjectFilesApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.GuideLink as GuideLink
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.File as File
import Common.Routes as Routes
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Uuid exposing (Uuid)


type alias Model =
    { files : Listing.Model ProjectFile
    , fileToBeDeleted : Maybe ProjectFile
    , deletingFile : ActionResult ()
    }


initialModel : Model
initialModel =
    { files = Listing.initialModel PaginationQueryString.empty
    , fileToBeDeleted = Nothing
    , deletingFile = Unset
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init pqs =
    let
        ( files, cmd ) =
            Listing.init pqs PaginationQueryFilters.empty
    in
    ( { initialModel | files = files }, Cmd.map ListingMsg cmd )



-- UPDATE


type Msg
    = ListingMsg (Listing.Msg ProjectFile)
    | DownloadFile ProjectFile
    | GotFileUrlCompleted (Result ApiError UrlResponse)
    | ShowHideDeleteFile (Maybe ProjectFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
    | RowAction Msg
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( files, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.files
            in
            ( { model | files = files }, cmd )

        DownloadFile file ->
            ( model
            , ProjectFilesApi.getFileUrl appState file.project.uuid file.uuid (cfg.wrapMsg << GotFileUrlCompleted)
            )

        GotFileUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( model, File.downloadFile urlResponse.url )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        ShowHideDeleteFile mbFile ->
            ( { model | fileToBeDeleted = mbFile, deletingFile = Unset }
            , case ( mbFile, model.fileToBeDeleted ) of
                ( Nothing, Just item ) ->
                    focusRowActions cfg item.uuid

                _ ->
                    Cmd.none
            )

        DeleteFileConfirm ->
            case model.fileToBeDeleted of
                Just file ->
                    ( { model | deletingFile = Loading }
                    , ProjectFilesApi.delete appState file.project.uuid file.uuid (cfg.wrapMsg << DeleteFileCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteFileCompleted result ->
            case result of
                Ok _ ->
                    ( { model | fileToBeDeleted = Nothing, deletingFile = Unset }
                    , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    )

                Err error ->
                    ( { model | deletingFile = ApiError.toActionResult appState (gettext "File could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        RowAction rowMsg ->
            -- The row menu closes under the dialog or the download, without
            -- taking focus back.
            let
                ( files, _ ) =
                    Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.files
            in
            update cfg appState rowMsg { model | files = files }

        NoOp ->
            ( model, Cmd.none )


{-| A dialog a row's action opened was dismissed and the row is still there:
focus goes back to the row's menu, which the dialog had made inert. A task, so
it runs once the dialog has closed.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig ProjectFile msg
listingUpdateConfig cfg appState =
    { getRequest = ProjectFilesApi.getList appState
    , getError = gettext "Unable to get project files." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.projectFilesIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.files)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Listing.viewWith appState
        { notices = [], overlays = [ deleteModal appState model ] }
        (listingConfig appState)
        model.files


listingConfig : AppState -> Listing.ViewConfig ProjectFile Msg
listingConfig appState =
    { title = gettext "Project Files" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsFiles ]
    , searchPlaceholder = gettext "Search files..." appState.locale
    , sortOptions =
        [ ( "createdAt", gettext "Created" appState.locale )
        , ( "fileName", gettext "File name" appState.locale )
        , ( "fileSize", gettext "File size" appState.locale )
        ]
    , filters = []
    , item = listingItem appState
    , emptyTitle = gettext "No project files" appState.locale
    , emptyDescription = gettext "There are no project files." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> ProjectFile -> Listing.Item Msg
listingItem appState file =
    let
        row =
            Listing.item
                { id = Uuid.toString file.uuid
                , title = fileTitle appState file
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ file.fileName ]
                }

        userFragment =
            case file.createdBy of
                Just user ->
                    -- One line: the icon is a block, so beside plain text it
                    -- broke onto a line of its own.
                    Layout.inline
                        [ ItemIcon.toHtml
                            (UserAvatar.user
                                { firstName = user.firstName
                                , lastName = user.lastName
                                , imageUrl = user.imageUrl
                                , gravatarHash = Just user.gravatarHash
                                }
                                |> ItemIcon.withSize ItemIcon.Small
                            )
                        , text (User.fullName user)
                        ]

                Nothing ->
                    Layout.inline
                        [ ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small UserAvatar.anonymous)
                        , text (gettext "Anonymous user" appState.locale)
                        ]
    in
    { row
        | icon = Just (ItemIcon.toHtml (UserAvatar.file file))
        , fragments =
            [ text (ByteUnits.toReadable file.fileSize)
            , Link.toHtml
                (Link.link
                    { label = file.project.name
                    , url = Routes.toUrl appState (Routes.projectsDetail file.project.uuid)
                    }
                )
            , userFragment
            ]
        , timestamp = Just (Timestamp.created appState file.createdAt)
        , actions =
            ItemActions.groups
                [ [ ( ItemActions.action
                        { icon = Icon.solid "fa-download"
                        , label = gettext "Download" appState.locale
                        , onSelect = RowAction (DownloadFile file)
                        }
                        |> ItemActions.withTestId "listing-item_action_download"
                    , True
                    )
                  ]
                , [ ( ItemActions.dangerAction
                        { icon = Icon.solid "fa-trash"
                        , label = gettext "Delete" appState.locale
                        , onSelect = RowAction (ShowHideDeleteFile (Just file))
                        }
                        |> ItemActions.withTestId "listing-item_action_delete"
                    , True
                    )
                  ]
                ]
    }


{-| A file's name downloads it, as its Download action does.
-}
fileTitle : AppState -> ProjectFile -> Html Msg
fileTitle appState file =
    Link.actionWithDescription "listing-item_title-download"
        { id = "file-title-" ++ Uuid.toString file.uuid
        , label = file.fileName
        , onClick = DownloadFile file
        , description = gettext "Click to download" appState.locale
        }


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete file" appState.locale
        , open = Maybe.isJust model.fileToBeDeleted
        , question = gettext "Are you sure you want to permanently delete this file?" appState.locale
        , entity =
            EntityCard.slot
                (Maybe.map
                    (\file ->
                        { icon = UserAvatar.file file
                        , name = file.fileName
                        , badges = []
                        , code = Nothing
                        , detail = Nothing
                        }
                    )
                    model.fileToBeDeleted
                )
        , onConfirm = DeleteFileConfirm
        , onClose = ShowHideDeleteFile Nothing
        , result = model.deletingFile
        , testId = Just "file-delete"
        }
