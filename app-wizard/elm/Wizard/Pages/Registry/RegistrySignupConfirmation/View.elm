module Wizard.Pages.Registry.RegistrySignupConfirmation.View exposing (view)

import Common.Components.FontAwesome exposing (faSuccess)
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import String.Format as String
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Registry.RegistrySignupConfirmation.Models exposing (Model)
import Wizard.Pages.Registry.RegistrySignupConfirmation.Msgs exposing (Msg)
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center" ]
        [ Page.actionResultView appState (successView appState) model.confirmation ]


successView : AppState -> () -> Html Msg
successView appState _ =
    div [ class "px-4 py-5 bg-light rounded-3" ]
        [ h1 [ class "display-3" ] [ faSuccess ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Your email was successfully confirmed, you can go back to %s." appState.locale)
                [ linkTo Routes.settingsRegistry
                    []
                    [ text (gettext "Settings" appState.locale) ]
                ]
            )
        ]
