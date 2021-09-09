module Wizard.Settings.Projects.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div, hr)
import Html.Attributes exposing (class)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Projects.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Projects.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableQuestionnairesConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form FormError EditableQuestionnairesConfigForm -> Html Form.Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "feedbackEnabled" form).value

        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "questionnaireSharingEnabled" form).value

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

        anonymousProjectEnabledInput =
            if sharingEnabled then
                [ FormGroup.toggle form "questionnaireSharingAnonymousEnabled" (l_ "form.questionnaireSharingAnonymous" appState)
                , FormExtra.mdAfter (l_ "form.questionnaireSharingAnonymous.desc" appState)
                ]

            else
                []
    in
    div []
        ([ FormGroup.toggle form "questionnaireVisibilityEnabled" (l_ "form.questionnaireVisibility" appState)
         , FormExtra.mdAfter (l_ "form.questionnaireVisibility.desc" appState)
         , FormGroup.richRadioGroup appState (QuestionnaireVisibility.richFormOptions appState) form "questionnaireVisibilityDefaultValue" (l_ "form.questionnaireVisibilityDefaultValue" appState)
         , FormExtra.mdAfter (l_ "form.questionnaireVisibilityDefaultValue.desc" appState)
         , hr [] []
         , FormGroup.toggle form "questionnaireSharingEnabled" (l_ "form.questionnaireSharing" appState)
         , FormExtra.mdAfter (l_ "form.questionnaireSharing.desc" appState)
         , FormGroup.richRadioGroup appState (QuestionnaireSharing.richFormOptions appState) form "questionnaireSharingDefaultValue" (l_ "form.questionnaireSharingDefaultValue" appState)
         , FormExtra.mdAfter (l_ "form.questionnaireSharingDefaultValue.desc" appState)
         ]
            ++ anonymousProjectEnabledInput
            ++ [ hr [] []
               , FormGroup.richRadioGroup appState (QuestionnaireCreation.richFormOptions appState) form "questionnaireCreation" (l_ "form.questionnaireCreation" appState)
               , hr [] []
               , FormGroup.toggle form "summaryReport" (l_ "form.summaryReport" appState)
               , FormExtra.mdAfter (l_ "form.summaryReport.desc" appState)
               , FormGroup.toggle form "feedbackEnabled" (l_ "form.feedback" appState)
               , FormExtra.mdAfter (l_ "form.feedback.desc" appState)
               , feedbackInput
               ]
        )
