module Common.Pages.KnowledgeModels.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The list of knowledge model packages.
-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
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
import Common.Pages.KnowledgeModels.Components.KnowledgeModelActions as KnowledgeModelActions
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
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


type alias Model =
    { packages : Listing.Model KnowledgeModelPackage
    , deleteModalModel : DeleteModal.Model
    , updatingKmPackagePhase : ActionResult ()
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { packages = Listing.initialModel paginationQueryString
    , deleteModalModel = DeleteModal.initialModel True
    , updatingKmPackagePhase = Unset
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init paginationQueryString =
    let
        ( packages, listingCmd ) =
            Listing.init paginationQueryString PaginationQueryFilters.empty
    in
    ( { packages = packages
      , deleteModalModel = DeleteModal.initialModel True
      , updatingKmPackagePhase = Unset
      }
    , Cmd.map ListingMsg listingCmd
    )


type Msg
    = DeleteModalMsg DeleteModal.Msg
    | ListingMsg (Listing.Msg KnowledgeModelPackage)
    | UpdatePhase KnowledgeModelPackage KnowledgeModelPackagePhase
    | UpdatePhaseCompleted (Result ApiError ())
    | UpdatePublic KnowledgeModelPackage Bool
    | UpdatePublicCompleted (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackage
    | ExportKnowledgeModelPackagePot KnowledgeModelPackage
    | ExportCompleted KnowledgeModelActions.ExportMsg
    | OpenDeleteModal KnowledgeModelPackage
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        DeleteModalMsg deleteModalMsg ->
            let
                deleteModalConfig =
                    { afterDeleteCmd = Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    , afterCloseCmd =
                        case model.deleteModalModel.toBeDeleted of
                            Just kmPackage ->
                                focusRowActions cfg kmPackage

                            Nothing ->
                                Cmd.none
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModalModel, deleteModalCmd ) =
                    DeleteModal.update appState deleteModalConfig deleteModalMsg model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }
            , deleteModalCmd
            )

        ListingMsg listingMsg ->
            let
                ( packages, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.packages
            in
            ( { model | packages = packages }, cmd )

        UpdatePhase kmPackage phase ->
            ( model, KnowledgeModelPackagesApi.putKnowledgeModelPackage appState { kmPackage | phase = phase } (cfg.wrapMsg << UpdatePhaseCompleted) )

        UpdatePhaseCompleted result ->
            handleUpdateCompleted cfg appState model result

        UpdatePublic kmPackage isPublic ->
            ( model, KnowledgeModelPackagesApi.putKnowledgeModelPackage appState { kmPackage | public = isPublic } (cfg.wrapMsg << UpdatePublicCompleted) )

        UpdatePublicCompleted result ->
            handleUpdateCompleted cfg appState model result

        ExportKnowledgeModelPackage kmPackage ->
            withMenusClosed cfg
                appState
                ( model, KnowledgeModelActions.export appState kmPackage.uuid (cfg.wrapMsg << ExportCompleted) )

        ExportKnowledgeModelPackagePot kmPackage ->
            withMenusClosed cfg
                appState
                ( model, KnowledgeModelActions.exportPot appState kmPackage.uuid (cfg.wrapMsg << ExportCompleted) )

        OpenDeleteModal kmPackage ->
            withMenusClosed cfg
                appState
                (update cfg appState (DeleteModalMsg (DeleteModal.open (DeleteModal.target DeleteModal.KnowledgeModel kmPackage))) model)

        ExportCompleted result ->
            ( model, KnowledgeModelActions.exportUpdate result )

        NoOp ->
            ( model, Cmd.none )


{-| The delete dialog a row's action opened was dismissed: focus goes back to
the row's menu.
-}
focusRowActions : UpdateConfig Msg msg -> { a | uuid : Uuid } -> Cmd msg
focusRowActions cfg kmPackage =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString kmPackage.uuid)))


{-| An export or a dialog leaves the row's menu open under it unless the listing
closes it.
-}
withMenusClosed : UpdateConfig Msg msg -> AppState -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
withMenusClosed cfg appState ( model, cmd ) =
    let
        ( packages, listingCmd ) =
            Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.packages
    in
    ( { model | packages = packages }, Cmd.batch [ cmd, listingCmd ] )


handleUpdateCompleted : UpdateConfig Msg msg -> AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handleUpdateCompleted cfg appState model result =
    case result of
        Ok _ ->
            -- A background reload keeps the rows, so focus stays where it was.
            ( model
            , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.reloadBackground))
            )

        Err error ->
            ( { model | updatingKmPackagePhase = ApiError.toActionResult appState (gettext "Knowledge model could not be updated." appState.locale) error }
            , RequestHelpers.getResultCmd cfg.logoutMsg result
            )


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig KnowledgeModelPackage msg
listingUpdateConfig cfg appState =
    { getRequest = KnowledgeModelPackagesApi.getKnowledgeModelPackages appState
    , getError = gettext "Unable to get Knowledge Models." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.knowledgeModelsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.packages)


view : RowPolicy -> AppState -> Model -> Html Msg
view policy appState model =
    Listing.viewWith appState
        { notices = []
        , overlays = [ Html.map DeleteModalMsg <| DeleteModal.view policy appState model.deleteModalModel ]
        }
        (listingConfig policy appState)
        model.packages


listingConfig : RowPolicy -> AppState -> Listing.ViewConfig KnowledgeModelPackage Msg
listingConfig policy appState =
    let
        importButton =
            if Feature.knowledgeModelsImport appState then
                [ Button.linkPrimary (gettext "Import" appState.locale) (Routes.toUrl appState (Routes.knowledgeModelsImport Nothing))
                    |> Button.withIcon (Icon.solid "fa-upload")
                ]

            else
                []
    in
    { title = gettext "Knowledge Models" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = importButton
    , searchPlaceholder = gettext "Search KMs..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , item = listingItem policy appState
    , emptyTitle = gettext "No data" appState.locale
    , emptyDescription =
        if Feature.knowledgeModelsImport appState then
            gettext "Click \"Import\" button to import a new knowledge model." appState.locale

        else
            gettext "There are no knowledge models." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : RowPolicy -> AppState -> KnowledgeModelPackage -> Listing.Item Msg
listingItem policy appState kmPackage =
    let
        row =
            Listing.item
                { id = Uuid.toString kmPackage.uuid
                , title =
                    Link.link
                        { label = kmPackage.name
                        , url = Routes.toUrl appState (Routes.knowledgeModelsDetail kmPackage.uuid)
                        }
                        |> Link.toHtml
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ kmPackage.name ]
                }

        badges =
            Maybe.values
                [ Just
                    (Badge.neutral (Version.toString kmPackage.version)
                        |> Badge.withDescription (gettext "Latest version" appState.locale)
                        |> Badge.withTestId "listing-item_badge_version"
                    )
                , badgeIf kmPackage.nonEditable (Badge.withTestId "listing-item_badge_non-editable" (Badge.neutral (gettext "non-editable" appState.locale)))
                , badgeIf (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated) (Badge.withTestId "listing-item_badge_deprecated" (Badge.danger (gettext "deprecated" appState.locale)))
                , badgeIf kmPackage.public (Badge.withTestId "listing-item_badge_public" (Badge.info (gettext "public" appState.locale)))
                , Maybe.map RowBadge.badge (policy.badge kmPackage.workspaceUuid)
                ]

        outdatedFragment =
            if KnowledgeModelPackage.isOutdated kmPackage then
                let
                    kmPackageId =
                        Maybe.map ((++) (kmPackage.id ++ ":") << Version.toString) kmPackage.remoteLatestVersion
                in
                [ Link.link
                    { label = gettext "update available" appState.locale
                    , url = Routes.toUrl appState (Routes.knowledgeModelsImport kmPackageId)
                    }
                    |> Link.toHtml
                ]

            else
                []
    in
    { row
        | icon = Just (ItemIcon.toHtml (UserAvatar.item kmPackage.name Nothing))
        , badges = badges
        , fragments =
            Text.code (KnowledgeModelUtils.getPackageIdWithoutVersion kmPackage)
                :: outdatedFragment
        , description = kmPackage.description
        , timestamp = Just (Timestamp.created appState kmPackage.createdAt)
        , actions =
            KnowledgeModelActions.actions policy
                appState
                { exportMsg = ExportKnowledgeModelPackage
                , exportPotMsg = ExportKnowledgeModelPackagePot
                , updatePhaseMsg = UpdatePhase
                , updatePublicMsg = UpdatePublic
                , deleteMsg = OpenDeleteModal
                , viewActionVisible = True
                }
                kmPackage
    }


badgeIf : Bool -> Badge.Badge msg -> Maybe (Badge.Badge msg)
badgeIf condition badge =
    if condition then
        Just badge

    else
        Nothing
