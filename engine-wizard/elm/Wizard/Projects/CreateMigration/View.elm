module Wizard.Projects.CreateMigration.View exposing (view)

import Form
import Html exposing (Html, div, label, option, text)
import Html.Attributes exposing (class, selected, value)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.Package exposing (Package)
import Shared.Data.PackageSuggestion as PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lg, lx)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.CreateMigration.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.CreateMigration.View"


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
            ( "", "--" ) :: List.map (createVersionOption package) package.versions

        tags =
            KnowledgeModel.getTags questionnaire.knowledgeModel

        originalTagList =
            div [ class "form-group form-group-tags" ]
                [ label [] [ lx_ "form.originalTags" appState ]
                , div [] [ Tag.readOnlyList appState questionnaire.selectedTagUuids tags ]
                ]

        cfg =
            { viewItem = TypeHintItem.packageSuggestion
            , wrapMsg = PackageTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.packageTypeHintInputModel

        versionSelect =
            case model.selectedPackage of
                Just package ->
                    FormGroup.select appState (createOptions package) model.form "packageId"

                Nothing ->
                    FormGroup.textView "km" <| l_ "form.selectKMFirst" appState
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.header (l_ "header.title" appState) []
        , Flash.info appState <| l_ "header.info" appState
        , FormResult.view appState model.savingMigration
        , FormGroup.textView "project" questionnaire.name <| lg "project" appState
        , div [ class "form" ]
            [ div []
                [ FormGroup.plainGroup
                    (TypeHintItem.packageSuggestion (PackageSuggestion.fromPackage questionnaire.package))
                    (l_ "form.originalKM" appState)
                , FormGroup.codeView (Version.toString questionnaire.package.version) <| l_ "form.originalVersion" appState
                , originalTagList
                ]
            , faSet "_global.arrowRight" appState
            , div []
                [ div [ class "form-group" ]
                    [ label [] [ lx_ "form.newKM" appState ]
                    , typeHintInput False
                    ]
                , Html.map FormMsg <| versionSelect <| l_ "form.newVersion" appState
                , tagsView appState model
                ]
            ]
        , FormActions.view appState
            Routes.projectsIndex
            (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingMigration (FormMsg Form.Submit) False)
        ]


tagsView : AppState -> Model -> Html Msg
tagsView appState model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            }
    in
    Tag.selection appState tagListConfig model.knowledgeModelPreview
