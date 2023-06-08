module Wizard.Projects.CreateMigration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.PackageSuggestion as PackageSuggestion
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (faSet)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.Projects.CreateMigration.Models exposing (Model)
import Wizard.Projects.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (createMigrationView appState model) model.questionnaire


createMigrationView : AppState -> Model -> QuestionnaireDetail -> Html Msg
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

        tags =
            KnowledgeModel.getTags questionnaire.knowledgeModel

        originalTagList =
            div [ class "form-group form-group-tags" ]
                [ label [] [ text (gettext "Original question tags" appState.locale) ]
                , div [] [ Tag.readOnlyList appState questionnaire.selectedQuestionTagUuids tags ]
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
                            FormGroup.select appState (createOptions selectedPackageDetail) model.form "packageId"

                        _ ->
                            always (Flash.loader appState)

                Nothing ->
                    FormGroup.textView "km" <| gettext "Select Knowledge Model first" appState.locale
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.header (gettext "Create Migration" appState.locale) []
        , Flash.info appState <| gettext "A new Project is created for the migration. The original will remain unchanged until the migration is finished." appState.locale
        , FormResult.view appState model.savingMigration
        , FormGroup.textView "project" questionnaire.name <| gettext "Project" appState.locale
        , div [ class "form" ]
            [ div []
                [ FormGroup.plainGroup
                    (TypeHintItem.packageSuggestion False (PackageSuggestion.fromPackage questionnaire.package))
                    (gettext "Original Knowledge Model" appState.locale)
                , FormGroup.codeView (Version.toString questionnaire.package.version) <| gettext "Original version" appState.locale
                , originalTagList
                ]
            , faSet "_global.arrowRight" appState
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
            (Routes.projectsIndex appState)
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
