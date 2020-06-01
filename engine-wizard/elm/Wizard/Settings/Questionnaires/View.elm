module Wizard.Settings.Questionnaires.View exposing (view)

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
import Wizard.Questionnaires.Common.QuestionnaireVisibility as QuestionnaireVisibility
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Questionnaires.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Questionnaires.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Questionnaires.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableQuestionnairesConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError EditableQuestionnairesConfigForm -> Html Form.Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "feedbackEnabled" form).value

        feedbackInput =
            if enabled then
                div [ class "nested-group" ]
                    [ FormGroup.input appState form "feedbackOwner" (l_ "form.feedbackOwner" appState)
                    , FormExtra.mdAfter (l_ "form.feedbackOwner.desc" appState)
                    , FormGroup.input appState form "feedbackRepo" (l_ "form.feedbackRepo" appState)
                    , FormExtra.mdAfter (l_ "form.feedbackRepo.desc" appState)
                    , FormGroup.textarea appState form "feedbackToken" (l_ "form.feedbackToken" appState)
                    , FormExtra.mdAfter (l_ "form.feedbackToken.desc" appState)
                    ]

            else
                emptyNode
    in
    div []
        [ FormGroup.toggle form "questionnaireVisibilityEnabled" (l_ "form.questionnaireVisibility" appState)
        , FormExtra.mdAfter (l_ "form.questionnaireVisibility.desc" appState)
        , FormGroup.select appState (QuestionnaireVisibility.formOptions appState) form "questionnaireVisibilityDefaultValue" (l_ "form.questionnaireVisibilityDefaultValue" appState)
        , FormExtra.mdAfter (l_ "form.questionnaireVisibilityDefaultValue.desc" appState)
        , FormGroup.toggle form "levelsEnabled" (l_ "form.phases" appState)
        , FormExtra.mdAfter (l_ "form.phases.desc" appState)
        , FormGroup.toggle form "feedbackEnabled" (l_ "form.feedback" appState)
        , FormExtra.mdAfter (l_ "form.feedback.desc" appState)
        , feedbackInput
        ]
