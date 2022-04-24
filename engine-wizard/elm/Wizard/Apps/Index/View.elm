module Wizard.Apps.Index.View exposing (view)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, target)
import Shared.Data.App exposing (App)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Apps.Index.Models exposing (Model)
import Wizard.Apps.Index.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (Route(..), indexRouteEnabledFilterId)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.AppIcon as AppIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Apps.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Apps.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "Apps__Index" ]
        [ Page.header (lg "apps" appState) []
        , Listing.view appState (listingConfig appState) model.apps
        ]


listingConfig : AppState -> ViewConfig App Msg
listingConfig appState =
    let
        enabledFilter =
            Listing.SimpleFilter indexRouteEnabledFilterId
                { name = l_ "filter.enabled.name" appState
                , options =
                    [ ( "true", l_ "filter.enabled.enabledOnly" appState )
                    , ( "false", l_ "filter.enabled.disabledOnly" appState )
                    ]
                }
    in
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = always []
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just AppIcon.view
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "app.name" appState )
        , ( "appId", lg "app.appId" appState )
        , ( "createdAt", lg "app.createdAt" appState )
        , ( "updatedAt", lg "app.updatedAt" appState )
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
                span [ class "badge badge-danger" ] [ lx_ "badge.disabled" appState ]

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
        [ lx_ "header.create" appState ]
