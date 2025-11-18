module Wizard.Pages.Projects.CreateMigration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.ActionButton as ActionResult
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faArrowRight)
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Version
import Wizard.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Components.FormActions as FormActions
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
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
        createVersionOption kmPackage version =
            let
                versionString =
                    Version.toString version

                kmPackageId =
                    String.join ":" <|
                        List.take 2 (String.split ":" kmPackage.id)
                            ++ [ versionString ]
            in
            ( kmPackageId, versionString )

        createOptions kmPackage =
            ( "", "--" ) :: List.map (createVersionOption kmPackage) (List.reverse (List.sortWith Version.compare kmPackage.versions))

        originalTagList =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (gettext "Original question tags" appState.locale) ]
                , div [] [ Tag.readOnlyList appState questionnaire.selectedQuestionTagUuids questionnaire.knowledgeModelTags ]
                ]

        cfg =
            { viewItem = TypeHintInputItem.packageSuggestion False
            , wrapMsg = KnowledgeModelPackageTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            , locale = appState.locale
            }

        typeHintInput =
            TypeHintInput.view cfg model.knowledgeModelPackageTypeHintInputModel

        versionSelect =
            case model.selectedPackage of
                Just _ ->
                    case model.selectedPackageDetail of
                        Success selectedPackageDetail ->
                            FormGroup.select appState.locale (createOptions selectedPackageDetail) model.form "knowledgeModelPackageId"

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
                    (TypeHintInputItem.packageSuggestion False (KnowledgeModelPackageSuggestion.fromKnowledgeModelPackage questionnaire.knowledgeModelPackage))
                    (gettext "Original Knowledge Model" appState.locale)
                , FormGroup.codeView (Version.toString questionnaire.knowledgeModelPackage.version) (gettext "Original Version" appState.locale)
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
