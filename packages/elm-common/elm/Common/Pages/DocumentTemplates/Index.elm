module Common.Pages.DocumentTemplates.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The list of document templates.
-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
import Common.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Components.LibraryDeleteModal as DeleteModal
import Common.Components.Listing as Listing
import Common.Components.RowBadge as RowBadge
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.DocumentTemplates.Components.DocumentTemplateActions as DocumentTemplateActions
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { documentTemplates : Listing.Model DocumentTemplate
    , deleteModalModel : DeleteModal.Model
    , updatingPhase : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { documentTemplates = Listing.initialModel paginationQueryString
    , deleteModalModel = DeleteModal.initialModel True
    , updatingPhase = Unset
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init paginationQueryString =
    let
        ( listing, listingCmd ) =
            Listing.init paginationQueryString PaginationQueryFilters.empty
    in
    ( { documentTemplates = listing
      , deleteModalModel = DeleteModal.initialModel True
      , updatingPhase = Unset
      }
    , Cmd.map ListingMsg listingCmd
    )



-- UPDATE


type Msg
    = OpenDeleteModal DocumentTemplate
    | DeleteModalMsg DeleteModal.Msg
    | ListingMsg (Listing.Msg DocumentTemplate)
    | UpdatePhase DocumentTemplate DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportDocumentTemplate DocumentTemplate
    | ExportDocumentTemplatePot DocumentTemplate
    | DownloadUrlCompleted (Result ApiError UrlResponse)
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        OpenDeleteModal documentTemplate ->
            update cfg appState (DeleteModalMsg (DeleteModal.open (DeleteModal.target DeleteModal.DocumentTemplate documentTemplate))) model
                |> withMenusClosed cfg appState

        DeleteModalMsg deleteModalMsg ->
            let
                deleteModalConfig =
                    { afterDeleteCmd = Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    , afterCloseCmd = Maybe.unwrap Cmd.none (focusRowActions cfg << .uuid) model.deleteModalModel.toBeDeleted
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModalModel, deleteModalCmd ) =
                    DeleteModal.update appState deleteModalConfig deleteModalMsg model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }, deleteModalCmd )

        ListingMsg listingMsg ->
            let
                ( documentTemplates, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.documentTemplates
            in
            ( { model | documentTemplates = documentTemplates }, cmd )

        UpdatePhase documentTemplate phase ->
            ( model
            , DocumentTemplatesApi.putTemplate appState { documentTemplate | phase = phase } (cfg.wrapMsg << UpdatePhaseCompleted)
            )

        UpdatePhaseCompleted result ->
            case result of
                Ok _ ->
                    ( model, Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete)) )

                Err error ->
                    ( { model | updatingPhase = ApiError.toActionResult appState (gettext "Document template could not be updated." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ExportDocumentTemplate documentTemplate ->
            ( model
            , DocumentTemplateActions.fetchDownloadUrl appState
                (DocumentTemplatesApi.exportTemplateUrl documentTemplate.uuid)
                (cfg.wrapMsg << DownloadUrlCompleted)
            )
                |> withMenusClosed cfg appState

        ExportDocumentTemplatePot documentTemplate ->
            ( model
            , DocumentTemplateActions.fetchDownloadUrl appState
                (DocumentTemplatesApi.exportTemplatePotUrl documentTemplate.uuid)
                (cfg.wrapMsg << DownloadUrlCompleted)
            )
                |> withMenusClosed cfg appState

        DownloadUrlCompleted result ->
            ( model, DocumentTemplateActions.downloadCompleted cfg.logoutMsg result )

        NoOp ->
            ( model, Cmd.none )


{-| An export or a dialog leaves the row's menu open under it unless the listing
closes it.
-}
withMenusClosed : UpdateConfig Msg msg -> AppState -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
withMenusClosed cfg appState ( model, cmd ) =
    let
        ( documentTemplates, listingCmd ) =
            Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.documentTemplates
    in
    ( { model | documentTemplates = documentTemplates }, Cmd.batch [ cmd, listingCmd ] )


{-| A dialog a row's action opened was dismissed and the row is still there:
focus goes back to the row's menu, which the dialog had made inert. A task, so
it runs once the dialog has closed.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig DocumentTemplate msg
listingUpdateConfig cfg appState =
    { getRequest = DocumentTemplatesApi.getTemplates appState
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.documentTemplatesIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.documentTemplates)



-- VIEW


view : RowPolicy -> AppState -> Model -> Html Msg
view policy appState model =
    Listing.viewWith appState
        { notices = []
        , overlays = [ Html.map DeleteModalMsg <| DeleteModal.view policy appState model.deleteModalModel ]
        }
        (listingConfig policy appState)
        model.documentTemplates


listingConfig : RowPolicy -> AppState -> Listing.ViewConfig DocumentTemplate Msg
listingConfig policy appState =
    let
        importButton =
            if Feature.documentTemplatesImport appState then
                [ Button.linkPrimary (gettext "Import" appState.locale) (Routes.toUrl appState (Routes.documentTemplatesImport Nothing))
                    |> Button.withIcon (Icon.solid "fa-upload")
                ]

            else
                []
    in
    { title = gettext "Document Templates" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = importButton
    , searchPlaceholder = gettext "Search..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , item = listingItem policy appState
    , emptyTitle = gettext "No document templates" appState.locale
    , emptyDescription =
        if Feature.documentTemplatesImport appState then
            gettext "Click \"Import\" button to import a new document template." appState.locale

        else
            gettext "There are no document templates." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : RowPolicy -> AppState -> DocumentTemplate -> Listing.Item Msg
listingItem policy appState documentTemplate =
    let
        base =
            Listing.item
                { id = Uuid.toString documentTemplate.uuid
                , title =
                    Link.link
                        { label = documentTemplate.name
                        , url = Routes.toUrl appState (Routes.documentTemplatesDetail documentTemplate.uuid)
                        }
                        |> Link.toHtml
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ documentTemplate.name ]
                }

        actions =
            DocumentTemplateActions.actions policy
                appState
                { exportMsg = ExportDocumentTemplate
                , exportPotMsg = ExportDocumentTemplatePot
                , updatePhaseMsg = UpdatePhase
                , deleteMsg = OpenDeleteModal
                , viewActionVisible = True
                }
                documentTemplate
    in
    { base
        | icon = Just (ItemIcon.toHtml (UserAvatar.item documentTemplate.name Nothing))
        , badges = listingBadges policy appState documentTemplate
        , fragments = listingFragments appState documentTemplate
        , description = documentTemplate.description
        , timestamp = Just (Timestamp.created appState documentTemplate.createdAt)
        , actions = actions
    }


listingBadges : RowPolicy -> AppState -> DocumentTemplate -> List (Badge.Badge msg)
listingBadges policy appState documentTemplate =
    Maybe.values
        [ Just
            (Badge.neutral (Version.toString documentTemplate.version)
                |> Badge.withDescription (gettext "Latest version" appState.locale)
                |> Badge.withTestId "listing-item_badge_version"
            )
        , if documentTemplate.nonEditable then
            Just (Badge.withTestId "listing-item_badge_non-editable" (Badge.neutral (gettext "non-editable" appState.locale)))

          else
            Nothing
        , if documentTemplate.state == DocumentTemplateState.UnsupportedMetamodelVersion then
            Just (Badge.withTestId "listing-item_badge_unsupported-metamodel" (Badge.danger (gettext "unsupported metamodel" appState.locale)))

          else
            Nothing
        , if documentTemplate.phase == DocumentTemplatePhase.Deprecated then
            Just (Badge.withTestId "listing-item_badge_deprecated" (Badge.danger (gettext "deprecated" appState.locale)))

          else
            Nothing
        , Maybe.map RowBadge.badge (policy.badge documentTemplate.workspaceUuid)
        , if DocumentTemplate.isOutdated documentTemplate then
            Just (Badge.withTestId "listing-item_badge_update-available" (Badge.warning (gettext "update available" appState.locale)))

          else
            Nothing
        ]


listingFragments : AppState -> DocumentTemplate -> List (Html Msg)
listingFragments appState documentTemplate =
    let
        -- The "update available" badge is not a link, so the link to the
        -- import of the newer version is here.
        outdatedFragment =
            if DocumentTemplate.isOutdated documentTemplate && Feature.documentTemplatesImport appState then
                let
                    documentTemplateId =
                        Maybe.map
                            ((++) (documentTemplate.id ++ ":") << Version.toString)
                            documentTemplate.remoteLatestVersion
                in
                [ Link.link
                    { label = gettext "Import" appState.locale
                    , url = Routes.toUrl appState (Routes.documentTemplatesImport documentTemplateId)
                    }
                    |> Link.withIcon (Icon.solid "fa-cloud-arrow-down")
                    |> Link.toHtml
                ]

            else
                []
    in
    outdatedFragment
        ++ [ Text.code (DocumentTemplateUtils.getIdWithoutVersion documentTemplate)
           , Layout.inline
                [ Icon.toHtml (Icon.labeled (gettext "Language" appState.locale) (Icon.solid "fa-language"))
                , text documentTemplate.language
                ]
           ]
