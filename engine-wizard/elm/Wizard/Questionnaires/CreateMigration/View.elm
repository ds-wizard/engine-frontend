module Wizard.Questionnaires.CreateMigration.View exposing (view)

import ActionResult
import Form
import Html exposing (Html, div, label, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.Locale exposing (l, lg, lx)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.CreateMigration.Models exposing (Model)
import Wizard.Questionnaires.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.CreateMigration.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Questionnaires.CreateMigration.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (createMigrationView appState model) <| ActionResult.combine model.questionnaire model.packages


createMigrationView : AppState -> Model -> ( QuestionnaireDetail, List Package ) -> Html Msg
createMigrationView appState model ( questionnaire, packages ) =
    let
        createVersionOption package version =
            let
                versionString =
                    Version.toString version

                packageId =
                    String.join ":" [ package.organizationId, package.kmId, versionString ]
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

        versionSelect =
            case model.selectedPackage of
                Just package ->
                    FormGroup.select appState (createOptions package) model.form "packageId"

                Nothing ->
                    FormGroup.textView <| l_ "form.selectKMFirst" appState
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.header (l_ "header.title" appState) []
        , Flash.info appState <| l_ "header.info" appState
        , FormResult.view appState model.savingMigration
        , FormGroup.textView questionnaire.name <| lg "questionnaire" appState
        , div [ class "form" ]
            [ div []
                [ FormGroup.textView questionnaire.package.name <| l_ "form.originalKM" appState
                , FormGroup.codeView (Version.toString questionnaire.package.version) <| l_ "form.originalVersion" appState
                , originalTagList
                ]
            , faSet "_global.arrowRight" appState
            , div []
                [ div [ class "form-group" ]
                    [ label [] [ lx_ "form.newKM" appState ]
                    , select [ class "form-control", onInput SelectPackage ]
                        (List.map (packageToOption model.selectedPackage) <| List.sortBy (String.toLower << .name) packages)
                    ]
                , Html.map FormMsg <| versionSelect <| l_ "form.newVersion" appState
                , tagsView appState model
                ]
            ]
        , FormActions.view appState
            (Routes.QuestionnairesRoute IndexRoute)
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


packageToOption : Maybe Package -> Package -> Html Msg
packageToOption selectedPackage package =
    option [ value package.id, selected <| selectedPackage == Just package ]
        [ text package.name ]
