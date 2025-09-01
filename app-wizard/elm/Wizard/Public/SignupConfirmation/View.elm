module Wizard.Public.SignupConfirmation.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Shared.Components.FontAwesome exposing (faSuccess)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page
import Wizard.Public.SignupConfirmation.Models exposing (Model)
import Wizard.Public.SignupConfirmation.Msgs exposing (Msg)
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
