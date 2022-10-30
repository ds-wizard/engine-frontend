module Wizard.Settings.Projects.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, hr)
import Html.Attributes exposing (class)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Projects.Models exposing (Model)


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableQuestionnairesConfigForm Msg
viewProps =
    { locTitle = gettext "Projects"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError EditableQuestionnairesConfigForm -> Html Form.Msg
formView appState form =
    let
        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "questionnaireSharingEnabled" form).value

        anonymousProjectEnabledInput =
            if sharingEnabled then
                [ FormGroup.toggle form "questionnaireSharingAnonymousEnabled" (gettext "Anonymous Projects" appState.locale)
                , FormExtra.mdAfter (gettext "If enabled, users that are not logged in are allowed to create projects from public knowledge models." appState.locale)
                ]

            else
                []

        feedbackEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "feedbackEnabled" form).value

        feedbackInput =
            if feedbackEnabled then
                div [ class "nested-group" ]
                    [ FormGroup.input appState form "feedbackOwner" (gettext "GitHub Repository Owner" appState.locale)
                    , FormExtra.mdAfter (gettext "ht&#8203;tps://github.com/**exampleOwner**/exampleRepository" appState.locale)
                    , FormGroup.input appState form "feedbackRepo" (gettext "GitHub Repository Name" appState.locale)
                    , FormExtra.mdAfter (gettext "ht&#8203;tps://github.com/exampleOwner/**exampleRepository**" appState.locale)
                    , FormGroup.textarea appState form "feedbackToken" (gettext "Access Token" appState.locale)
                    , FormExtra.mdAfter (gettext "[Personal Access Token](https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line) for GitHub account creating the issues. Make sure to select **public_repo** scope if the repository is public or **repo** if it is private. We recommend creating a bot account with the access to the feedback repository instead of using your own GitHub account." appState.locale)
                    ]

            else
                emptyNode

        projectTaggingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "projectTaggingEnabled" form).value

        projectTaggingInput =
            if projectTaggingEnabled then
                div [ class "nested-group" ]
                    [ FormGroup.resizableTextarea appState form "projectTaggingTags" (gettext "Default Project Tags" appState.locale)
                    , FormExtra.mdAfter (gettext "Default projects tags can be provided to users so they don't have to come up with them on their own. Write one tag per line." appState.locale)
                    ]

            else
                emptyNode
    in
    div []
        ([ FormGroup.toggle form "questionnaireVisibilityEnabled" (gettext "Project Visibility" appState.locale)
         , FormExtra.mdAfter (gettext "If enabled, project visibility can be set for each project. Otherwise, all projects use the *Default Project Visibility*." appState.locale)
         , FormGroup.richRadioGroup appState (QuestionnaireVisibility.richFormOptions appState) form "questionnaireVisibilityDefaultValue" (gettext "Default Project Visibility" appState.locale)
         , FormExtra.mdAfter (gettext "If *Project Visibility* is enabled, this value is pre-selected when creating a new project. If *Project Visibility* is disabled, this value is used implicitly for new projects." appState.locale)
         , hr [] []
         , FormGroup.toggle form "questionnaireSharingEnabled" (gettext "Project Sharing" appState.locale)
         , FormExtra.mdAfter (gettext "If enabled, users can choose if project can be shared with people outside of DSW or not. Otherwise, all projects use the *Default Project Sharing*." appState.locale)
         , FormGroup.richRadioGroup appState (QuestionnaireSharing.richFormOptions appState) form "questionnaireSharingDefaultValue" (gettext "Default Project Sharing" appState.locale)
         , FormExtra.mdAfter (gettext "If *Project Sharing* is enabled, this value is pre-selected when creating a new project. If *Project Sharing* is disabled, this value is used implicitly for new projects." appState.locale)
         ]
            ++ anonymousProjectEnabledInput
            ++ [ hr [] []
               , FormGroup.richRadioGroup appState (QuestionnaireCreation.richFormOptions appState) form "questionnaireCreation" (gettext "Project Creation" appState.locale)
               , hr [] []
               , FormGroup.toggle form "summaryReport" (gettext "Summary Report" appState.locale)
               , FormExtra.mdAfter (gettext "If enabled, users can view a summary report showing various metrics for the questionnaire." appState.locale)
               , FormGroup.toggle form "feedbackEnabled" (gettext "Feedback" appState.locale)
               , FormExtra.mdAfter (gettext "If enabled, users can submit a feedback to questions to the specified GitHub repository." appState.locale)
               , feedbackInput
               , FormGroup.toggle form "projectTaggingEnabled" (gettext "Project Tagging" appState.locale)
               , FormExtra.mdAfter (gettext "If enabled, users can tag their projects and use these tags to filter them." appState.locale)
               , projectTaggingInput
               ]
        )
