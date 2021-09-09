module Wizard.KMEditor.Editor.Settings.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchEditForm exposing (BranchEditForm)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.Settings.View"


view : AppState -> Form FormError BranchEditForm -> Html Form.Msg
view appState form =
    div [ class "KMEditor__Editor__SettingsEditor", dataCy "km-editor_settings" ]
        [ div [ detailClass "" ]
            [ Page.header (l_ "title" appState) []
            , div []
                [ FormGroup.input appState form "name" (l_ "form.name" appState)
                , FormGroup.input appState form "kmId" (l_ "form.kmId" appState)
                ]
            ]
        ]
