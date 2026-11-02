module Common.Pages.Dashboard.Widgets.OutdatedTemplates exposing
    ( Model
    , Msg
    , enabled
    , fetchData
    , initialModel
    , update
    , view
    )

{-| Document templates with a newer version available in the registry.

Shown only when there is something to update, as in the wizard.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.WizardRolePermission as RolePermission
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setDocumentTemplates)
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
    AppState.userHasPerm RolePermission.documentTemplatesManage appState


type alias Model =
    { documentTemplates : ActionResult (List DocumentTemplate)
    }


initialModel : Model
initialModel =
    { documentTemplates = ActionResult.Loading }


type Msg
    = GetDocumentTemplatesCompleted (Result ApiError (Pagination DocumentTemplate))


fetchData : AppState -> Cmd Msg
fetchData appState =
    DocumentTemplatesApi.getOutdatedTemplates appState GetDocumentTemplatesCompleted


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetDocumentTemplatesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setDocumentTemplates
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = appState.locale
                }


view : AppState -> Model -> Maybe (Widget msg)
view appState model =
    case model.documentTemplates of
        Success ((_ :: _) as templates) ->
            Widget.widget
                { title = gettext "Update Document Templates" appState.locale
                , body = [ ListItem.list (List.map (viewDocumentTemplate appState) templates) ]
                }
                |> Widget.withDescription (gettext "There are updates available for some document templates." appState.locale)
                |> Just

        _ ->
            Nothing


viewDocumentTemplate : AppState -> DocumentTemplate -> ListItem.ListItem msg
viewDocumentTemplate appState template =
    ListItem.item
        (Layout.inline
            [ Link.toHtml (Link.link { label = template.name, url = Routes.toUrlIn appState template.workspaceUuid (Routes.documentTemplatesDetail template.uuid) })
            , Badge.toHtml (Badge.warning (gettext "update available" appState.locale))
            ]
        )
        |> ListItem.withIcon (ItemIcon.toHtml (UserAvatar.item template.name Nothing))
        |> ListItem.withFragments [ Text.code (DocumentTemplateUtils.getId template) ]
        |> ListItem.withTestId ("outdated-templates_item_" ++ Uuid.toString template.uuid)
