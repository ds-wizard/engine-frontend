module Common.Pages.Settings.Projects exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Project.ProjectCreation as ProjectCreation
import Common.Api.Models.Project.ProjectSharing as ProjectSharing
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility
import Common.Api.Models.Settings.ProjectsSettings as ProjectsSettings exposing (ProjectsSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.ProjectsSettingsForm as ProjectsSettingsForm exposing (ProjectsSettingsForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Field as UiField
import Ui.Form as UiForm
import Ui.Layout as Layout
import Ui.Separator as Separator


type alias Model =
    SettingsPage.Model ProjectsSettings ProjectsSettingsForm


type alias Msg =
    SettingsPage.Msg ProjectsSettings


initialModel : AppState -> Model
initialModel appState =
    SettingsPage.initialModel (ProjectsSettingsForm.initEmpty appState)


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState ProjectsSettings.section (ProjectsSettingsForm.initEmpty appState)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = ProjectsSettings.section
        , initForm = ProjectsSettingsForm.init appState
        , formToValue = \form _ -> ProjectsSettingsForm.toSettings form
        , formValidation = ProjectsSettingsForm.validation appState
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Projects" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsProjects)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError ProjectsSettingsForm -> Html Msg
formView appState form =
    let
        isOn path =
            Maybe.withDefault False (Form.getFieldAsBool path form).value

        toggle path label hintText =
            Field.toggle appState form path label
                |> Field.withHint hintText
                |> Field.toHtml

        toggleMarkdown path label hintText =
            Field.toggle appState form path label
                |> Field.withHintMarkdown hintText
                |> Field.toHtml

        choices =
            List.map
                (\( value, label, description ) ->
                    UiField.choice { value = value, label = label }
                        |> UiField.withChoiceDescription description
                )

        anonymousProjectEnabledInput =
            if isOn "questionnaireSharingEnabled" then
                [ UiForm.nested
                    [ toggle "questionnaireSharingAnonymousEnabled"
                        (gettext "Anonymous Projects" appState.locale)
                        (gettext "If enabled, users that are not logged in are allowed to create projects from public knowledge models." appState.locale)
                    ]
                ]

            else
                []

        projectTaggingInput =
            if isOn "projectTaggingEnabled" then
                [ UiForm.nested
                    [ Field.textarea appState form "projectTaggingTags" (gettext "Default Project Tags" appState.locale)
                        |> Field.withHint (gettext "Default projects tags can be provided to users so they don't have to come up with them on their own. Write one tag per line." appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ([ toggleMarkdown "questionnaireVisibilityEnabled"
                (gettext "Project Visibility" appState.locale)
                (gettext "If enabled, project visibility can be set for each project. Otherwise, all projects use the *Default Project Visibility*." appState.locale)
             , Field.radioGroup appState form "questionnaireVisibilityDefaultValue" (gettext "Default Project Visibility" appState.locale) (choices (ProjectVisibility.richFormOptions appState))
                |> Field.withHintMarkdown (gettext "If *Project Visibility* is enabled, this value is pre-selected when creating a new project. If *Project Visibility* is disabled, this value is used implicitly for new projects." appState.locale)
                |> Field.toHtml
             , Separator.line
             , toggleMarkdown "questionnaireSharingEnabled"
                (gettext "Project Sharing" appState.locale)
                (gettext "If enabled, users can choose if project can be shared with people outside of the wizard or not. Otherwise, all projects use the *Default Project Sharing*." appState.locale)
             ]
                ++ anonymousProjectEnabledInput
                ++ [ Field.radioGroup appState form "questionnaireSharingDefaultValue" (gettext "Default Project Sharing" appState.locale) (choices (ProjectSharing.richFormOptions appState))
                        |> Field.withHintMarkdown (gettext "If *Project Sharing* is enabled, this value is pre-selected when creating a new project. If *Project Sharing* is disabled, this value is used implicitly for new projects." appState.locale)
                        |> Field.toHtml
                   , Separator.line
                   , Field.radioGroup appState form "questionnaireCreation" (gettext "Project Creation" appState.locale) (choices (ProjectCreation.richFormOptions appState))
                        |> Field.toHtml
                   , Separator.line
                   , toggle "summaryReport"
                        (gettext "Summary Report" appState.locale)
                        (gettext "If enabled, users can view a summary report showing various metrics for the questionnaire." appState.locale)
                   , toggle "projectTaggingEnabled"
                        (gettext "Project Tagging" appState.locale)
                        (gettext "If enabled, users can tag their projects and use these tags to filter them." appState.locale)
                   ]
                ++ projectTaggingInput
            )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
