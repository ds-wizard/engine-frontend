module Wizard.Apps.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, target)
import Shared.Components.Badge as Badge
import Shared.Data.App exposing (App)
import Shared.Html exposing (emptyNode)
import Wizard.Apps.Index.Models exposing (Model)
import Wizard.Apps.Index.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (indexRouteEnabledFilterId)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.AppIcon as AppIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "Apps__Index" ]
        [ Page.header (gettext "Apps" appState.locale) []
        , Listing.view appState (listingConfig appState) model.apps
        ]


listingConfig : AppState -> ViewConfig App Msg
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
    , emptyText = gettext "Click \"Create\" button to add a new App." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just AppIcon.view
    , searchPlaceholderText = Just (gettext "Search apps..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "appId", gettext "App ID" appState.locale )
        , ( "createdAt", gettext "Created at" appState.locale )
        , ( "updatedAt", gettext "Updated at" appState.locale )
        ]
    , filters =
        [ enabledFilter
        ]
    , toRoute = Routes.appsIndexWithFilters
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> App -> Html Msg
listingTitle appState app =
    let
        disabledBadge =
            if not app.enabled then
                Badge.danger [] [ text (gettext "Disabled" appState.locale) ]

            else
                emptyNode
    in
    span []
        [ linkTo appState (Routes.appsDetail app.uuid) [] [ text app.name ]
        , disabledBadge
        ]


listingDescription : App -> Html Msg
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
        Routes.appsCreate
        [ class "btn btn-primary"
        ]
        [ text (gettext "Create" appState.locale) ]
