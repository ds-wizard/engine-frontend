module Wizard.Settings.KnowledgeModelRegistry.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, button, div, label)
import Html.Attributes exposing (attribute, class, placeholder)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.KnowledgeModelRegistry.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.KnowledgeModelRegistry.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.KnowledgeModelRegistry.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableKnowledgeModelRegistryConfig
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError EditableKnowledgeModelRegistryConfig -> Html Form.Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        tokenInput =
            if enabled then
                div [ class "nested-group" ]
                    [ FormGroup.textarea appState form "token" (l_ "form.token" appState)
                    , FormExtra.mdAfter (l_ "form.token.desc" appState)
                    ]

            else
                emptyNode
    in
    div []
        [ FormGroup.toggle form "enabled" (l_ "form.enabled" appState)
        , FormExtra.mdAfter (l_ "form.enabled.desc" appState)
        , tokenInput
        ]
