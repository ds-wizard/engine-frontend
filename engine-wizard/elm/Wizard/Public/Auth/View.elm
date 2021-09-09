module Wizard.Public.Auth.View exposing (view)

import Html exposing (Html)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (\_ -> emptyNode) model.authenticating
