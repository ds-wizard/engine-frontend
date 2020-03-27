module Wizard.Settings.View exposing (view)

import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Routes
import Wizard.Settings.Affiliation.View
import Wizard.Settings.Auth.View
import Wizard.Settings.Client.View
import Wizard.Settings.Features.View
import Wizard.Settings.Info.View
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.View
import Wizard.Settings.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.View"


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    let
        content =
            case route of
                AffiliationRoute ->
                    Html.map AffiliationMsg <|
                        Wizard.Settings.Affiliation.View.view appState model.affiliationModel

                AuthRoute ->
                    Html.map AuthMsg <|
                        Wizard.Settings.Auth.View.view appState model.authModel

                ClientRoute ->
                    Html.map ClientMsg <|
                        Wizard.Settings.Client.View.view appState model.clientModel

                FeaturesRoute ->
                    Html.map FeaturesMsg <|
                        Wizard.Settings.Features.View.view appState model.featuresModel

                InfoRoute ->
                    Html.map InfoMsg <|
                        Wizard.Settings.Info.View.view appState model.infoModel

                OrganizationRoute ->
                    Html.map OrganizationMsg <|
                        Wizard.Settings.Organization.View.view appState model.organizationModel
    in
    div [ class "Settings" ]
        [ div [ class "Settings__navigation" ] [ navigation appState route ]
        , div [ class "Settings__content" ] [ content ]
        ]


navigation : AppState -> Route -> Html Msg
navigation appState currentRoute =
    div [ class "nav nav-pills flex-column" ]
        ([ strong [] [ lx_ "navigation.title" appState ] ]
            ++ List.map (navigationLink appState currentRoute) (navigationLinks appState)
        )


navigationLinks : AppState -> List ( Route, String )
navigationLinks appState =
    [ ( FeaturesRoute, l_ "navigation.features" appState )
    , ( AuthRoute, l_ "navigation.auth" appState )
    , ( ClientRoute, l_ "navigation.client" appState )
    , ( InfoRoute, l_ "navigation.info" appState )
    , ( AffiliationRoute, l_ "navigation.affiliation" appState )
    , ( OrganizationRoute, l_ "navigation.organization" appState )
    ]


navigationLink : AppState -> Route -> ( Route, String ) -> Html Msg
navigationLink appState currentRoute ( route, title ) =
    linkTo appState
        (Wizard.Routes.SettingsRoute route)
        [ class "nav-link", classList [ ( "active", currentRoute == route ) ] ]
        [ text title ]
