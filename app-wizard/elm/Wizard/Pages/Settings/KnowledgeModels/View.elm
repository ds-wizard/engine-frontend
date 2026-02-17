module Wizard.Pages.Settings.KnowledgeModels.View exposing (view)

import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
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
    div []
        [ FormGroup.resizableTextarea appState.locale form "integrationConfig" (gettext "Integration Config" appState.locale)
        , FormExtra.mdAfter
            (String.format
                (gettext "Integration config is used to configure [secrets and other properties](%s) for integrations in knowledge models." appState.locale)
                [ WizardGuideLinks.integrationQuestionSecrets appState.guideLinks ]
            )
        ]
