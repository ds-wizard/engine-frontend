module Questionnaires.CreateMigration.View exposing (view)

import ActionResult
import Common.Html exposing (fa)
import Common.Html.Attribute exposing (listClass)
import Common.View.ActionButton as ActionResult
import Common.View.Flash as Flash
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Common.View.Tag as Tag
import Form
import Html exposing (Html, div, label, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Common.Version as Version
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.CreateMigration.Models exposing (Model)
import Questionnaires.CreateMigration.Msgs exposing (Msg(..))
import Questionnaires.Routing
import Routing


view : Model -> Html Msg
view model =
    Page.actionResultView (createMigrationView model) <| ActionResult.combine model.questionnaire model.packages


createMigrationView : Model -> ( QuestionnaireDetail, List Package ) -> Html Msg
createMigrationView model ( questionnaire, packages ) =
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
                [ label [] [ text "Original tags" ]
                , div [] [ Tag.readOnlyList questionnaire.selectedTagUuids tags ]
                ]

        versionSelect =
            case model.selectedPackage of
                Just package ->
                    FormGroup.select (createOptions package) model.form "packageId"

                Nothing ->
                    FormGroup.textView "Select Knowledge Model first"
    in
    div [ listClass "Questionnaires__CreateMigration" ]
        [ Page.header "Create migration" []
        , Flash.info "New questionnaire is created for the migration. The original will remain unchanged."
        , FormResult.view model.savingMigration
        , FormGroup.textView questionnaire.name "Questionnaire"
        , div [ class "form" ]
            [ div []
                [ FormGroup.textView questionnaire.package.name "Original Knowledge Model"
                , FormGroup.codeView (Version.toString questionnaire.package.version) "Original Version"
                , originalTagList
                ]
            , fa "arrow-right"
            , div []
                [ div [ class "form-group" ]
                    [ label [] [ text "New Knowledge Model" ]
                    , select [ class "form-control", onInput SelectPackage ]
                        (List.map (packageToOption model.selectedPackage) <| List.sortBy (String.toLower << .name) packages)
                    ]
                , Html.map FormMsg <| versionSelect "New Version"
                , tagsView model
                ]
            ]
        , FormActions.view
            (Routing.Questionnaires Questionnaires.Routing.Index)
            (ActionResult.ButtonConfig "Create" model.savingMigration (FormMsg Form.Submit) False)
        ]


tagsView : Model -> Html Msg
tagsView model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            }
    in
    Tag.selection tagListConfig model.knowledgeModelPreview


packageToOption : Maybe Package -> Package -> Html Msg
packageToOption selectedPackage package =
    option [ value package.id, selected <| selectedPackage == Just package ]
        [ text package.name ]
