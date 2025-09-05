module Wizard.Pages.Public.SignupConfirmation.View exposing (view)

import Common.Components.FontAwesome exposing (faSuccess)
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import String.Format as String
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.SignupConfirmation.Models exposing (Model)
import Wizard.Pages.Public.SignupConfirmation.Msgs exposing (Msg)
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center Public__SignupConfirmation" ]
        [ Page.actionResultView appState (successView appState) model.confirmation ]


successView : AppState -> String -> Html Msg
successView appState _ =
    div [ class "px-4 py-5 bg-light rounded-3e", dataCy "message_success" ]
        [ h1 [ class "display-3" ] [ faSuccess ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Your email was successfully confirmed. You can now %s." appState.locale)
                [ linkTo (Routes.publicLogin Nothing)
                    [ class "btn btn-primary ms-1" ]
                    [ text (gettext "log in" appState.locale) ]
                ]
            )
        ]
