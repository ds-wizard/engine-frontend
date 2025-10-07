module Wizard.Pages.Settings.Projects.View exposing (view)

import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, hr)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Html.Keyed
import Wizard.Api.Models.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Pages.Settings.Projects.Models exposing (Model)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps EditableQuestionnairesConfigForm Msg
viewProps =
    { locTitle = gettext "Projects"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsProjects
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError EditableQuestionnairesConfigForm -> Html Form.Msg
formView appState form =
    let
        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "questionnaireSharingEnabled" form).value

        anonymousProjectEnabledInput =
            if sharingEnabled then
                [ ( "questionnaireSharingAnonymousEnabled", FormGroup.toggle form "questionnaireSharingAnonymousEnabled" (gettext "Anonymous Projects" appState.locale) )
                , ( "questionnaireSharingAnonymousEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users that are not logged in are allowed to create projects from public knowledge models." appState.locale) )
                ]

            else
                []

        feedbackEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "feedbackEnabled" form).value

        feedbackInput =
            if feedbackEnabled then
                ( "feedback"
                , div [ class "nested-group" ]
                    [ FormGroup.input appState.locale form "feedbackOwner" (gettext "GitHub Repository Owner" appState.locale)
                    , FormExtra.mdAfter (gettext "ht&#8203;tps://github.com/**exampleOwner**/exampleRepository" appState.locale)
                    , FormGroup.input appState.locale form "feedbackRepo" (gettext "GitHub Repository Name" appState.locale)
                    , FormExtra.mdAfter (gettext "ht&#8203;tps://github.com/exampleOwner/**exampleRepository**" appState.locale)
                    , FormGroup.secret appState.locale form "feedbackToken" (gettext "Access Token" appState.locale)
                    , FormExtra.mdAfter (gettext "[Personal Access Token](https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line) for GitHub account creating the issues. Make sure to select **public_repo** scope if the repository is public or **repo** if it is private. We recommend creating a bot account with the access to the feedback repository instead of using your own GitHub account." appState.locale)
                    ]
                )

            else
                ( "feedback-nothing", Html.nothing )

        projectTaggingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "projectTaggingEnabled" form).value

        projectTaggingInput =
            if projectTaggingEnabled then
                ( "projectTaggingTags"
                , div [ class "nested-group" ]
                    [ FormGroup.resizableTextarea appState.locale form "projectTaggingTags" (gettext "Default Project Tags" appState.locale)
                    , FormExtra.mdAfter (gettext "Default projects tags can be provided to users so they don't have to come up with them on their own. Write one tag per line." appState.locale)
                    ]
                )

            else
                ( "projectTaggingTags-nothing", Html.nothing )
    in
    Html.Keyed.node "div"
        []
        ([ ( "questionnaireVisibilityEnabled", FormGroup.toggle form "questionnaireVisibilityEnabled" (gettext "Project Visibility" appState.locale) )
         , ( "questionnaireVisibilityEnabled-extra", FormExtra.mdAfter (gettext "If enabled, project visibility can be set for each project. Otherwise, all projects use the *Default Project Visibility*." appState.locale) )
         , ( "questionnaireVisibilityDefaultValue", FormGroup.richRadioGroup appState.locale (QuestionnaireVisibility.richFormOptions appState) form "questionnaireVisibilityDefaultValue" (gettext "Default Project Visibility" appState.locale) )
         , ( "questionnaireVisibilityDefaultValue-extra", FormExtra.mdAfter (gettext "If *Project Visibility* is enabled, this value is pre-selected when creating a new project. If *Project Visibility* is disabled, this value is used implicitly for new projects." appState.locale) )
         , ( "separator1", hr [] [] )
         , ( "questionnaireSharingEnabled", FormGroup.toggle form "questionnaireSharingEnabled" (gettext "Project Sharing" appState.locale) )
         , ( "questionnaireSharingEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users can choose if project can be shared with people outside of the wizard or not. Otherwise, all projects use the *Default Project Sharing*." appState.locale) )
         , ( "questionnaireSharingDefaultValue", FormGroup.richRadioGroup appState.locale (QuestionnaireSharing.richFormOptions appState) form "questionnaireSharingDefaultValue" (gettext "Default Project Sharing" appState.locale) )
         , ( "questionnaireSharingDefaultValue-extra", FormExtra.mdAfter (gettext "If *Project Sharing* is enabled, this value is pre-selected when creating a new project. If *Project Sharing* is disabled, this value is used implicitly for new projects." appState.locale) )
         ]
            ++ anonymousProjectEnabledInput
            ++ [ ( "separator2", hr [] [] )
               , ( "questionnaireCreation", FormGroup.richRadioGroup appState.locale (QuestionnaireCreation.richFormOptions appState) form "questionnaireCreation" (gettext "Project Creation" appState.locale) )
               , ( "separator3", hr [] [] )
               , ( "summaryReport", FormGroup.toggle form "summaryReport" (gettext "Summary Report" appState.locale) )
               , ( "summaryReport-extra", FormExtra.mdAfter (gettext "If enabled, users can view a summary report showing various metrics for the questionnaire." appState.locale) )
               , ( "feedbackEnabled", FormGroup.toggle form "feedbackEnabled" (gettext "Feedback" appState.locale) )
               , ( "feedbackEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users can submit a feedback to questions to the specified GitHub repository." appState.locale) )
               , feedbackInput
               , ( "projectTaggingEnabled", FormGroup.toggle form "projectTaggingEnabled" (gettext "Project Tagging" appState.locale) )
               , ( "projectTaggingEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users can tag their projects and use these tags to filter them." appState.locale) )
               , projectTaggingInput
               ]
        )
