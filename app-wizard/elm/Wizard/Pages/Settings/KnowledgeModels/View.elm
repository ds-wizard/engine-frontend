module Wizard.Pages.Settings.KnowledgeModels.View exposing (view)

import Common.Components.FontAwesome exposing (faDelete)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, hr, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.EditableKnowledgeModelConfigFrom exposing (EditableKnowledgeModelConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Pages.Settings.KnowledgeModels.Models exposing (Model)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableKnowledgeModelConfigForm Msg
viewProps =
    { locTitle = gettext "Knowledge Models"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsKnowledgeModels
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
                    [ FormGroup.listWithHeader appState.locale allowedInputHeader allowedPackageFormView form "publicPackages" (gettext "Allowed Packages" appState.locale) (gettext "Add knowledge model" appState.locale)
                    ]

            else
                Html.nothing
    in
    div []
        [ FormGroup.toggle form "publicEnabled" (gettext "Public Knowledge Models" appState.locale)
        , FormExtra.mdAfter (gettext "If enabled, you can define knowledge models that are publicly available for reading." appState.locale)
        , allowedInput
        , hr [] []
        , FormGroup.resizableTextarea appState.locale form "integrationConfig" (gettext "Integration Config" appState.locale)
        , FormExtra.mdAfter
            (String.format
                (gettext "Integration config is used to configure [secrets and other properties](%s) for integrations in knowledge models." appState.locale)
                [ WizardGuideLinks.integrationQuestionSecrets appState.guideLinks ]
            )
        ]


allowedPackageFormView : Form FormError EditableKnowledgeModelConfigForm -> Int -> Html Form.Msg
allowedPackageFormView form index =
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
        , a
            [ class "btn btn-link text-danger"
            , onClick (Form.RemoveItem "publicPackages" index)
            ]
            [ faDelete ]
        ]
