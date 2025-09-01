module Wizard.Pages.Projects.CreateMigration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Shared.Components.ActionButton as ActionResult
import Shared.Components.Flash as Flash
import Shared.Components.FontAwesome exposing (faArrowRight)
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Page as Page
import Version
import Wizard.Api.Models.PackageSuggestion as PackageSuggestion
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Components.FormActions as FormActions
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.CreateMigration.Models exposing (Model)
import Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (createMigrationView appState model) model.questionnaire


createMigrationView : AppState -> Model -> QuestionnaireSettings -> Html Msg
createMigrationView appState model questionnaire =
    let
        createVersionOption package version =
            let
                versionString =
                    Version.toString version

                packageId =
                    String.join ":" <|
                        List.take 2 (String.split ":" package.id)
                            ++ [ versionString ]
            in
            ( packageId, versionString )

        createOptions package =
            ( "", "--" ) :: List.map (createVersionOption package) (List.reverse (List.sortWith Version.compare package.versions))

        originalTagList =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (gettext "Original question tags" appState.locale) ]
                , div [] [ Tag.readOnlyList appState questionnaire.selectedQuestionTagUuids questionnaire.knowledgeModelTags ]
                ]

        cfg =
            { viewItem = TypeHintItem.packageSuggestion False
            , wrapMsg = PackageTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.packageTypeHintInputModel

        versionSelect =
            case model.selectedPackage of
                Just _ ->
                    case model.selectedPackageDetail of
                        Success selectedPackageDetail ->
                            FormGroup.select appState.locale (createOptions selectedPackageDetail) model.form "packageId"

                        _ ->
                            always (Flash.loader appState.locale)

                Nothing ->
                    FormGroup.textView "km" <| gettext "Select knowledge model first" appState.locale
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsMigration) (gettext "Create Migration" appState.locale)
        , Flash.info <| gettext "A new project is created for the migration. The original will remain unchanged until the migration is finished." appState.locale
        , FormResult.view model.savingMigration
        , FormGroup.textView "project" questionnaire.name <| gettext "Project" appState.locale
        , div [ class "form" ]
            [ div []
                [ FormGroup.plainGroup
                    (TypeHintItem.packageSuggestion False (PackageSuggestion.fromPackage questionnaire.package))
                    (gettext "Original Knowledge Model" appState.locale)
                , FormGroup.codeView (Version.toString questionnaire.package.version) (gettext "Original Version" appState.locale)
                , originalTagList
                ]
            , faArrowRight
            , div []
                [ div [ class "form-group" ]
                    [ label [] [ text (gettext "New Knowledge Model" appState.locale) ]
                    , typeHintInput False
                    ]
                , Html.map FormMsg <| versionSelect <| gettext "New version" appState.locale
                , tagsView appState model
                ]
            ]
        , FormActions.view appState
            Cancel
            (ActionResult.ButtonConfig (gettext "Create" appState.locale) model.savingMigration (FormMsg Form.Submit) False)
        ]


tagsView : AppState -> Model -> Html Msg
tagsView appState model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            , showDescription = True
            }

        selectionConfig =
            { tagListConfig = tagListConfig
            , useAllQuestions = model.useAllQuestions
            , useAllQuestionsMsg = ChangeUseAllQuestions
            }
    in
    Tag.selection appState selectionConfig model.knowledgeModelPreview
