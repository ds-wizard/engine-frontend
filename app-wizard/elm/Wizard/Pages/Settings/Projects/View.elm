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
import Wizard.Api.Models.Project.ProjectCreation as ProjectCreation
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility
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
         , ( "questionnaireVisibilityDefaultValue", FormGroup.richRadioGroup appState.locale (ProjectVisibility.richFormOptions appState) form "questionnaireVisibilityDefaultValue" (gettext "Default Project Visibility" appState.locale) )
         , ( "questionnaireVisibilityDefaultValue-extra", FormExtra.mdAfter (gettext "If *Project Visibility* is enabled, this value is pre-selected when creating a new project. If *Project Visibility* is disabled, this value is used implicitly for new projects." appState.locale) )
         , ( "separator1", hr [] [] )
         , ( "questionnaireSharingEnabled", FormGroup.toggle form "questionnaireSharingEnabled" (gettext "Project Sharing" appState.locale) )
         , ( "questionnaireSharingEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users can choose if project can be shared with people outside of the wizard or not. Otherwise, all projects use the *Default Project Sharing*." appState.locale) )
         , ( "questionnaireSharingDefaultValue", FormGroup.richRadioGroup appState.locale (ProjectSharing.richFormOptions appState) form "questionnaireSharingDefaultValue" (gettext "Default Project Sharing" appState.locale) )
         , ( "questionnaireSharingDefaultValue-extra", FormExtra.mdAfter (gettext "If *Project Sharing* is enabled, this value is pre-selected when creating a new project. If *Project Sharing* is disabled, this value is used implicitly for new projects." appState.locale) )
         ]
            ++ anonymousProjectEnabledInput
            ++ [ ( "separator2", hr [] [] )
               , ( "questionnaireCreation", FormGroup.richRadioGroup appState.locale (ProjectCreation.richFormOptions appState) form "questionnaireCreation" (gettext "Project Creation" appState.locale) )
               , ( "separator3", hr [] [] )
               , ( "summaryReport", FormGroup.toggle form "summaryReport" (gettext "Summary Report" appState.locale) )
               , ( "summaryReport-extra", FormExtra.mdAfter (gettext "If enabled, users can view a summary report showing various metrics for the questionnaire." appState.locale) )
               , ( "projectTaggingEnabled", FormGroup.toggle form "projectTaggingEnabled" (gettext "Project Tagging" appState.locale) )
               , ( "projectTaggingEnabled-extra", FormExtra.mdAfter (gettext "If enabled, users can tag their projects and use these tags to filter them." appState.locale) )
               , projectTaggingInput
               ]
        )
