module Questionnaires.Create.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg)
import Common.View.ActionButton as ActionResult
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Common.View.Tag as Tag
import Form exposing (Form)
import Html exposing (..)
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Common.Version as Version
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility
import Questionnaires.Create.Models exposing (Model)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Routes exposing (Route(..))
import Routes


l_ : String -> AppState -> String
l_ =
    l "Questionnaires.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.packages


content : AppState -> Model -> List Package -> Html Msg
content appState model packages =
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.view model.savingQuestionnaire
            , formView appState model packages |> Html.map FormMsg
            , tagsView appState model
            , FormActions.view appState
                (Routes.QuestionnairesRoute IndexRoute)
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

        accessibilitySelect =
            if appState.config.questionnaireAccessibilityEnabled then
                FormGroup.richRadioGroup appState QuestionnaireAccessibility.formOptions model.form "accessibility" <| lg "questionnaire.accessibility" appState

            else
                emptyNode

        formHtml =
            div []
                [ FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
                , parentInput <| lg "knowledgeModel" appState
                , accessibilitySelect
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
