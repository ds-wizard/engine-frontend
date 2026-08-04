module Wizard.Pages.Dashboard.Widgets.OutdatedPackagesWidget exposing (Model, Msg, UpdateConfig, enabled, fetchData, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.Badge as Badge
import Common.Data.WizardRolePermission as RolePermission
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelPackages)
import Gettext exposing (gettext)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


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


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetPackagesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setKnowledgeModelPackages
                , defaultError = gettext "Unable to get Knowledge Models." cfg.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = cfg.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    case model.knowledgeModelPackages of
        ActionResult.Success packageList ->
            if not (List.isEmpty packageList) then
                viewWidget appState packageList

            else
                Html.nothing

        _ ->
            Html.nothing


viewWidget : AppState -> List KnowledgeModelPackage -> Html msg
viewWidget appState kmPackages =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Update Knowledge Models" appState.locale) ]
            , div [ class "mb-4" ] [ text (gettext "There are updates available for some knowledge models." appState.locale) ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewKnowledgeModelPackage appState) kmPackages)
            ]
        ]


viewKnowledgeModelPackage : AppState -> KnowledgeModelPackage -> Html msg
viewKnowledgeModelPackage appState kmPackage =
    linkTo (Routes.knowledgeModelsDetail kmPackage.uuid)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = kmPackage.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text kmPackage.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text (KnowledgeModelUtils.getPackageId kmPackage) ]
                , Badge.warning [ class "ms-2" ] [ text (gettext "update available" appState.locale) ]
                ]
            ]
        ]
