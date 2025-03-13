module Wizard.Tenants.Index.View exposing (view)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Extra as Html
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.Tenant exposing (Tenant)
import Shared.Data.TenantState as TenantState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.Page as Page
import Wizard.Common.View.TenantIcon as TenantIcon
import Wizard.Routes as Routes
import Wizard.Tenants.Index.Models exposing (Model)
import Wizard.Tenants.Index.Msgs exposing (Msg(..))
import Wizard.Tenants.Routes exposing (indexRouteEnabledFilterId, indexRouteStatesFilterId)


view : AppState -> Model -> Html Msg
view appState model =
    let
        editWarning =
            Html.viewIf (Admin.isEnabled appState.config.admin) <|
                div [ class "alert alert-danger mt-n3 mb-4 d-flex align-items-center" ]
                    [ faSet "_global.warning" appState
                    , Markdown.toHtml [] (String.format "Do not edit tenants here. Go to [Admin Center](%s)." [ "/admin/tenants" ])
                    ]
    in
    div [ listClass "Tenants__Index" ]
        [ Page.header "Tenants" []
        , editWarning
        , Listing.view appState (listingConfig appState) model.tenants
        ]


listingConfig : AppState -> ViewConfig Tenant Msg
listingConfig appState =
    let
        enabledFilter =
            Listing.SimpleFilter indexRouteEnabledFilterId
                { name = "Enabled"
                , options =
                    [ ( "true", "Enabled only" )
                    , ( "false", "Disabled only" )
                    ]
                }

        stateFilter =
            Listing.SimpleMultiFilter indexRouteStatesFilterId
                { name = "State"
                , options = TenantState.filterOptions
                , maxVisibleValues = 1
                }
    in
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = always []
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new tenant."
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just TenantIcon.view
    , searchPlaceholderText = Just "Search tenants..."
    , sortOptions =
        [ ( "name", "Name" )
        , ( "tenantId", "Tenant ID" )
        , ( "createdAt", "Created at" )
        , ( "updatedAt", "Updated at" )
        ]
    , filters =
        [ enabledFilter
        , stateFilter
        ]
    , toRoute = Routes.tenantsIndexWithFilters
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> Tenant -> Html Msg
listingTitle appState app =
    let
        disabledBadge =
            if not app.enabled then
                Badge.danger [] [ text "Disabled" ]

            else
                emptyNode
    in
    span []
        [ linkTo appState (Routes.tenantsDetail app.uuid) [] [ text app.name ]
        , disabledBadge
        ]


listingDescription : Tenant -> Html Msg
listingDescription tenant =
    let
        visibleUrl =
            String.replace "https://" "" tenant.clientUrl

        tenantState =
            if tenant.state /= TenantState.ReadyForUse then
                span [ class "fragment" ]
                    [ span [ class "badge text-bg-light" ] [ text (TenantState.toReadableString tenant.state) ]
                    ]

            else
                Html.nothing
    in
    span []
        [ a [ href tenant.clientUrl, target "_blank", class "fragment" ] [ text visibleUrl ]
        , tenantState
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        Routes.tenantsCreate
        [ class "btn btn-primary"
        ]
        [ text "Create" ]
