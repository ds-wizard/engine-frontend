module Common.Pages.Dashboard.Widgets.OutdatedPackages exposing
    ( Model
    , Msg
    , enabled
    , fetchData
    , initialModel
    , update
    , view
    )

{-| Knowledge models with a newer version available in the registry.

Shown only when there is something to update, as in the wizard.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.WizardRolePermission as RolePermission
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelPackages)
import Gettext exposing (gettext)
import Ui.Badge as Badge
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Text as Text
import Ui.Widget as Widget exposing (Widget)
import Uuid


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.knowledgeModelsManage appState


type alias Model =
    { knowledgeModelPackages : ActionResult (List KnowledgeModelPackage)
    }


initialModel : Model
initialModel =
    { knowledgeModelPackages = ActionResult.Loading }


type Msg
    = GetPackagesCompleted (Result ApiError (Pagination KnowledgeModelPackage))


fetchData : AppState -> Cmd Msg
fetchData appState =
    KnowledgeModelPackagesApi.getOutdatedKnowledgeModelPackages appState GetPackagesCompleted


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetPackagesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setKnowledgeModelPackages
                , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = appState.locale
                }


view : AppState -> Model -> Maybe (Widget msg)
view appState model =
    case model.knowledgeModelPackages of
        Success ((_ :: _) as kmPackages) ->
            Widget.widget
                { title = gettext "Update Knowledge Models" appState.locale
                , body = [ ListItem.list (List.map (viewKnowledgeModelPackage appState) kmPackages) ]
                }
                |> Widget.withDescription (gettext "There are updates available for some knowledge models." appState.locale)
                |> Just

        _ ->
            Nothing


viewKnowledgeModelPackage : AppState -> KnowledgeModelPackage -> ListItem.ListItem msg
viewKnowledgeModelPackage appState kmPackage =
    ListItem.item
        (Layout.inline
            [ Link.toHtml (Link.link { label = kmPackage.name, url = Routes.toUrlIn appState kmPackage.workspaceUuid (Routes.knowledgeModelsDetail kmPackage.uuid) })
            , Badge.toHtml (Badge.warning (gettext "update available" appState.locale))
            ]
        )
        |> ListItem.withIcon (ItemIcon.toHtml (UserAvatar.item kmPackage.name Nothing))
        |> ListItem.withFragments [ Text.code (KnowledgeModelUtils.getPackageId kmPackage) ]
        |> ListItem.withTestId ("outdated-packages_item_" ++ Uuid.toString kmPackage.uuid)
