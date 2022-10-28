module Wizard.Settings.KnowledgeModels.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, button, div, hr, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.EditableKnowledgeModelConfigFrom exposing (EditableKnowledgeModelConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.KnowledgeModels.Models exposing (Model)


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableKnowledgeModelConfigForm Msg
viewProps =
    { locTitle = gettext "Knowledge Models"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError EditableKnowledgeModelConfigForm -> Html Form.Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "publicEnabled" form).value

        allowedInput =
            if enabled then
                let
                    allowedInputHeader =
                        div [ class "form-list-header mb-2" ]
                            [ span [] [ text (gettext "Organization ID" appState.locale) ]
                            , span [] [ text (gettext "Knowledge Model ID" appState.locale) ]
                            , span [] [ text (gettext "Min Version" appState.locale) ]
                            , span [] [ text (gettext "Max Version" appState.locale) ]
                            ]
                in
                div [ class "nested-group" ]
                    [ FormGroup.listWithHeader appState allowedInputHeader (allowedPackageFormView appState) form "publicPackages" (gettext "Allowed Packages" appState.locale)
                    ]

            else
                emptyNode
    in
    div []
        [ FormGroup.toggle form "publicEnabled" (gettext "Public Knowledge Models" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled you can define knowledge models that are publicly available for reading." appState.locale)
        , allowedInput
        , hr [] []
        , FormGroup.resizableTextarea appState form "integrationConfig" (gettext "Integration Config" appState.locale)
        ]


allowedPackageFormView : AppState -> Form FormError EditableKnowledgeModelConfigForm -> Int -> Html Form.Msg
allowedPackageFormView appState form index =
    let
        fieldName name =
            "publicPackages." ++ String.fromInt index ++ "." ++ name

        getField name =
            Form.getFieldAsString (fieldName name) form

        viewField name =
            Input.textInput (getField name) [ class "form-control", id (fieldName name) ]
    in
    div [ class "input-group mb-2" ]
        [ viewField "orgId"
        , viewField "kmId"
        , viewField "minVersion"
        , viewField "maxVersion"
        , button
            [ class "btn btn-link text-danger"
            , onClick (Form.RemoveItem "publicPackages" index)
            ]
            [ faSet "_global.delete" appState ]
        ]
