module Wizard.Pages.Dashboard.Widgets.OutdatedTemplatesWidget exposing (Model, Msg, UpdateConfig, enabled, fetchData, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.Badge as Badge
import Common.Data.WizardRolePermission as RolePermission
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setDocumentTemplates)
import Gettext exposing (gettext)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.documentTemplatesManage appState


type alias Model =
    { documentTemplates : ActionResult (List DocumentTemplate)
    }


initialModel : Model
initialModel =
    { documentTemplates = ActionResult.Loading
    }


type Msg
    = GetDocumentTemplatesCompleted (Result ApiError (Pagination DocumentTemplate))


fetchData : AppState -> Cmd Msg
fetchData appState =
    DocumentTemplatesApi.getOutdatedTemplates appState GetDocumentTemplatesCompleted


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetDocumentTemplatesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setDocumentTemplates
                , defaultError = gettext "Unable to get document templates." cfg.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = cfg.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    case model.documentTemplates of
        ActionResult.Success templateList ->
            if not (List.isEmpty templateList) then
                viewWidget appState templateList

            else
                Html.nothing

        _ ->
            Html.nothing


viewWidget : AppState -> List DocumentTemplate -> Html msg
viewWidget appState templates =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Update Document Templates" appState.locale) ]
            , div [ class "mb-4" ] [ text (gettext "There are updates available for some document templates." appState.locale) ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewTemplate appState) templates)
            ]
        ]


viewTemplate : AppState -> DocumentTemplate -> Html msg
viewTemplate appState template =
    linkTo (Routes.documentTemplatesDetail template.uuid)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = template.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text template.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text (DocumentTemplateUtils.getId template) ]
                , Badge.warning [ class "ms-2" ] [ text (gettext "update available" appState.locale) ]
                ]
            ]
        ]
