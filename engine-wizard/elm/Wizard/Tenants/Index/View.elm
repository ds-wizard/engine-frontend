module Wizard.Tenants.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Extra as Html
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.Tenant exposing (Tenant)
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
import Wizard.Tenants.Routes exposing (indexRouteEnabledFilterId)


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
        [ Page.header (gettext "Tenants" appState.locale) []
        , editWarning
        , Listing.view appState (listingConfig appState) model.tenants
        ]


listingConfig : AppState -> ViewConfig Tenant Msg
listingConfig appState =
    let
        enabledFilter =
            Listing.SimpleFilter indexRouteEnabledFilterId
                { name = gettext "Enabled" appState.locale
                , options =
                    [ ( "true", gettext "Enabled only" appState.locale )
                    , ( "false", gettext "Disabled only" appState.locale )
                    ]
                }
    in
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = always []
    , textTitle = .name
    , emptyText = gettext "Click \"Create\" button to add a new tenant." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just TenantIcon.view
    , searchPlaceholderText = Just (gettext "Search tenants..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "tenantId", gettext "Tenant ID" appState.locale )
        , ( "createdAt", gettext "Created at" appState.locale )
        , ( "updatedAt", gettext "Updated at" appState.locale )
        ]
    , filters =
        [ enabledFilter
        ]
    , toRoute = Routes.tenantsIndexWithFilters
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> Tenant -> Html Msg
listingTitle appState app =
    let
        disabledBadge =
            if not app.enabled then
                Badge.danger [] [ text (gettext "Disabled" appState.locale) ]

            else
                emptyNode
    in
    span []
        [ linkTo appState (Routes.tenantsDetail app.uuid) [] [ text app.name ]
        , disabledBadge
        ]


listingDescription : Tenant -> Html Msg
listingDescription app =
    let
        visibleUrl =
            String.replace "https://" "" app.clientUrl
    in
    span []
        [ a [ href app.clientUrl, target "_blank" ] [ text visibleUrl ]
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        Routes.tenantsCreate
        [ class "btn btn-primary"
        ]
        [ text (gettext "Create" appState.locale) ]
