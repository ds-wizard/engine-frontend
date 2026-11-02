module Common.Pages.DocumentTemplateEditors.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The list of document template editors.
-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Common.Api.Models.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { documentTemplateDrafts : Listing.Model DocumentTemplateDraft
    , documentTemplateDraftToBeDeleted : Maybe DocumentTemplateDraft
    , deletingDocumentTemplateDraft : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { documentTemplateDrafts = Listing.initialModel paginationQueryString
    , documentTemplateDraftToBeDeleted = Nothing
    , deletingDocumentTemplateDraft = Unset
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init paginationQueryString =
    let
        ( listing, listingCmd ) =
            Listing.init paginationQueryString PaginationQueryFilters.empty
    in
    ( { documentTemplateDrafts = listing
      , documentTemplateDraftToBeDeleted = Nothing
      , deletingDocumentTemplateDraft = Unset
      }
    , Cmd.map ListingMsg listingCmd
    )



-- UPDATE


type Msg
    = ShowHideDeleteDocumentTemplateDraft (Maybe DocumentTemplateDraft)
    | DeleteDocumentTemplateDraft
    | DeleteDocumentTemplateDraftCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplateDraft)
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ShowHideDeleteDocumentTemplateDraft template ->
            -- The dialog takes focus as it opens; the row's menu closes
            -- without taking it back.
            let
                ( documentTemplateDrafts, listingCmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.documentTemplateDrafts
            in
            ( { model
                | documentTemplateDraftToBeDeleted = template
                , deletingDocumentTemplateDraft = Unset
                , documentTemplateDrafts = documentTemplateDrafts
              }
            , Cmd.batch
                [ listingCmd
                , case ( template, model.documentTemplateDraftToBeDeleted ) of
                    ( Nothing, Just item ) ->
                        focusRowActions cfg item.uuid

                    _ ->
                        Cmd.none
                ]
            )

        DeleteDocumentTemplateDraft ->
            case model.documentTemplateDraftToBeDeleted of
                Just template ->
                    ( { model | deletingDocumentTemplateDraft = Loading }
                    , DocumentTemplateDraftsApi.deleteDraft appState template.uuid (cfg.wrapMsg << DeleteDocumentTemplateDraftCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteDocumentTemplateDraftCompleted result ->
            case result of
                Ok _ ->
                    ( { model | documentTemplateDraftToBeDeleted = Nothing }
                    , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    )

                Err error ->
                    ( { model | deletingDocumentTemplateDraft = ApiError.toActionResult appState (gettext "Document template editor could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ListingMsg listingMsg ->
            let
                ( documentTemplateDrafts, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.documentTemplateDrafts
            in
            ( { model | documentTemplateDrafts = documentTemplateDrafts }, cmd )

        NoOp ->
            ( model, Cmd.none )


{-| A dialog a row's action opened was dismissed and the row is still there:
focus goes back to the row's menu, which the dialog had made inert. A task, so
it runs once the dialog has closed.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig DocumentTemplateDraft msg
listingUpdateConfig cfg appState =
    { getRequest = DocumentTemplateDraftsApi.getDrafts appState
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.documentTemplateEditorsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.documentTemplateDrafts)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Listing.viewWith appState
        { notices = []
        , overlays = [ deleteModal appState model ]
        }
        (listingConfig appState)
        model.documentTemplateDrafts


listingConfig : AppState -> Listing.ViewConfig DocumentTemplateDraft Msg
listingConfig appState =
    let
        createButton =
            Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState (Routes.documentTemplateEditorCreate Nothing Nothing))
                |> Button.withIcon (Icon.solid "fa-plus")
                |> Button.withTestId "document-template-editors_create-button"
    in
    { title = gettext "Document Template Editors" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = [ createButton ]
    , searchPlaceholder = gettext "Search..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = []
    , item = listingItem appState
    , emptyTitle = gettext "No document template editors" appState.locale
    , emptyDescription = gettext "Click \"Create\" button to add a new document template editor." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> DocumentTemplateDraft -> Listing.Item Msg
listingItem appState template =
    let
        -- The editor itself is not on this interface yet; the link
        -- still goes to its route, which shows the placeholder.
        editorUrl =
            Routes.toUrl appState (Routes.documentTemplateEditorDetail template.uuid)

        base =
            Listing.item
                { id = Uuid.toString template.uuid
                , title = Link.toHtml (Link.link { label = template.name, url = editorUrl })
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ template.name ]
                }

        openAction =
            ItemActions.link
                { icon = Icon.solid "fa-pen-to-square"
                , label = gettext "Open editor" appState.locale
                , url = editorUrl
                }
                |> ItemActions.withTestId "listing-item_action_view"

        deleteAction =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = ShowHideDeleteDocumentTemplateDraft (Just template)
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    { base
        | icon = Just (ItemIcon.toHtml (UserAvatar.item template.name Nothing))
        , badges = [ Badge.withDescription (gettext "Latest version" appState.locale) (Badge.neutral (Version.toString template.version)) ]
        , fragments = [ Text.code (DocumentTemplateUtils.getId template) ]
        , description = template.description
        , timestamp = Just (Timestamp.updated appState template.updatedAt)
        , actions =
            ItemActions.groups
                [ [ ( openAction, Feature.documentTemplateEditorsView appState ) ]
                , [ ( deleteAction, Feature.documentTemplateEditorsDelete appState ) ]
                ]
    }


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete document template editor" appState.locale
        , open = Maybe.isJust model.documentTemplateDraftToBeDeleted
        , question = gettext "Are you sure you want to permanently delete this document template editor?" appState.locale
        , entity =
            EntityCard.slot
                (Maybe.map
                    (\template ->
                        { icon = UserAvatar.item template.name Nothing
                        , name = template.name
                        , badges = []
                        , code = Just (DocumentTemplateUtils.getId template)
                        , detail = Just template.description
                        }
                    )
                    model.documentTemplateDraftToBeDeleted
                )
        , onConfirm = DeleteDocumentTemplateDraft
        , onClose = ShowHideDeleteDocumentTemplateDraft Nothing
        , result = model.deletingDocumentTemplateDraft
        , testId = Just "templates-delete"
        }
