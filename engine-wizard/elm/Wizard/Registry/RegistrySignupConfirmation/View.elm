module Wizard.Registry.RegistrySignupConfirmation.View exposing (view)

import Html exposing (Html, div, h1, p)
import Html.Attributes exposing (class)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Registry.RegistrySignupConfirmation.Models exposing (Model)
import Wizard.Registry.RegistrySignupConfirmation.Msgs exposing (Msg)
import Wizard.Routes as Routes
import Wizard.Settings.Routes exposing (Route(..))


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Registry.RegistrySignupConfirmation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Registry.RegistrySignupConfirmation.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center" ]
        [ Page.actionResultView appState (successView appState) model.confirmation ]


successView : AppState -> () -> Html Msg
successView appState _ =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "confirmation"
                [ linkTo appState (Routes.SettingsRoute RegistryRoute) [] [ lx_ "settings" appState ]
                ]
                appState
            )
        ]
