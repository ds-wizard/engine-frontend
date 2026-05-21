module Wizard.Pages.Projects.CreateMigration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.ActionButton as ActionResult
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faArrowRight, faClose, faKmCompare)
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Gettext exposing (gettext)
import Html exposing (Html, button, div, hr, label, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Uuid
import Version
import Wizard.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Api.Models.VersionUuid as VersionUuid
import Wizard.Components.FormActions as FormActions
import Wizard.Components.KMComparison as KMComparison
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.CreateMigration.Models exposing (Model)
import Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (createMigrationView appState model) model.project


createMigrationView : AppState -> Model -> ProjectSettings -> Html Msg
createMigrationView appState model project =
    let
        createVersionOption version =
            ( Uuid.toString version.uuid, Version.toString version.version )

        createOptions kmPackage =
            ( "", "--" ) :: List.map createVersionOption (List.reverse (List.sortWith VersionUuid.compare kmPackage.versions))

        originalTagList =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (gettext "Original question tags" appState.locale) ]
                , div [] [ Tag.readOnlyList appState project.selectedQuestionTagUuids project.knowledgeModelTags ]
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
                            FormGroup.select appState.locale (createOptions selectedPackageDetail) model.form "knowledgeModelPackageUuid"

                        _ ->
                            always (Flash.loader appState.locale)

                Nothing ->
                    FormGroup.textView "km" <| gettext "Select knowledge model first" appState.locale

        compareButton =
            case model.knowledgeModelPreview of
                Success _ ->
                    div []
                        [ hr [] []
                        , button
                            [ class "btn btn-outline-secondary with-icon"
                            , onClick CompareKnowledgeModels
                            ]
                            [ faKmCompare
                            , text (gettext "Compare" appState.locale)
                            ]
                        ]

                _ ->
                    Html.nothing
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsMigration) (gettext "Create Migration" appState.locale)
        , Flash.info <| gettext "A new project is created for the migration. The original will remain unchanged until the migration is finished." appState.locale
        , FormResult.view model.savingMigration
        , FormGroup.textView "project" project.name <| gettext "Project" appState.locale
        , div [ class "form" ]
            [ div []
                [ FormGroup.plainGroup
                    (div [ class "typehint-input" ]
                        [ div [ class "typehint-input-value form-control cursor-default" ]
                            [ TypeHintInputItem.packageSuggestion False (KnowledgeModelPackageSuggestion.fromKnowledgeModelPackage project.knowledgeModelPackage) ]
                        ]
                    )
                    (gettext "Original Knowledge Model" appState.locale)
                , FormGroup.codeView (Version.toString project.knowledgeModelPackage.version) (gettext "Original Version" appState.locale)
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
                , compareButton
                ]
            ]
        , FormActions.view appState
            Cancel
            (ActionResult.ButtonConfig (gettext "Create" appState.locale) model.savingMigration (FormMsg Form.Submit) False)
        , compareModal appState model
        ]


compareModal : AppState -> Model -> Html Msg
compareModal appState model =
    let
        modalConfig =
            { modalContent =
                [ div [ class "modal-header" ]
                    [ strong [ class "modal-title" ] [ text (gettext "Compare Knowledge Models" appState.locale) ]
                    , button [ class "close", onClick CloseCompareModal ]
                        [ faClose ]
                    ]
                , div [ class "modal-body p-0 d-flex flex-column" ]
                    [ Html.map KMComparisonMsg <| KMComparison.view appState model.kmComparisonModel
                    ]
                ]
            , visible = model.compareModalOpen
            , enterMsg = Nothing
            , escMsg = Just CloseCompareModal
            , dataCy = "compare-modal"
            }
    in
    Modal.simpleWithAttrs [ class "modal-full-screen" ] modalConfig


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
