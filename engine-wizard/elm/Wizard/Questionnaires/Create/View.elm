module Wizard.Questionnaires.Create.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Shared.Data.Package exposing (Package)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lgh)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.Questionnaires.Create.Models exposing (Model)
import Wizard.Questionnaires.Create.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.packages


content : AppState -> Model -> List Package -> Html Msg
content appState model packages =
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.view appState model.savingQuestionnaire
            , formView appState model packages |> Html.map FormMsg
            , tagsView appState model
            , FormActions.view appState
                (Routes.QuestionnairesRoute (IndexRoute PaginationQueryString.empty))
                (ActionResult.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> List Package -> Html Form.Msg
formView appState model packages =
    let
        packageOptions =
            ( "", "--" ) :: (List.map createOption <| List.sortBy .name packages)

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.select appState packageOptions model.form "packageId"

        visibilityEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "visibilityEnabled" model.form).value

        visibilityEnabledInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                FormGroup.toggle model.form "visibilityEnabled" (lg "questionnaire.visibility" appState)

            else
                emptyNode

        visibilityPermissionInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", visibilityEnabled ) ]
                    ]
                    (lgh "questionnaire.visibilityPermission" [ visibilitySelect ] appState)

            else
                emptyNode

        visibilitySelect =
            if (Form.getFieldAsString "sharingPermission" model.form).value == Just "edit" then
                strong [] [ text "edit" ]

            else
                FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) model.form "visibilityPermission"

        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" model.form).value

        sharingEnabledInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                FormGroup.toggle model.form "sharingEnabled" (lg "questionnaire.sharing" appState)

            else
                emptyNode

        sharingPermissionInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", sharingEnabled ) ]
                    ]
                    (lgh "questionnaire.sharingPermission" [ sharingSelect ] appState)

            else
                emptyNode

        sharingSelect =
            FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) model.form "sharingPermission"

        formHtml =
            div []
                [ FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
                , parentInput <| lg "knowledgeModel" appState
                , visibilityEnabledInput
                , visibilityPermissionInput
                , sharingEnabledInput
                , sharingPermissionInput
                ]
    in
    formHtml


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


createOption : Package -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ Version.toString package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
