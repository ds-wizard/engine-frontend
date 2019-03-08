module Questionnaires.Create.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
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
import Msgs
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility
import Questionnaires.Create.Models exposing (Model, QuestionnaireCreateForm)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Routing
import Routing


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (content wrapMsg appState model) model.packages


content : (Msg -> Msgs.Msg) -> AppState -> Model -> List Package -> Html Msgs.Msg
content wrapMsg appState model packages =
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header "Create Questionnaire" []
        , div []
            [ FormResult.view model.savingQuestionnaire
            , formView appState model packages |> Html.map (wrapMsg << FormMsg)
            , tagsView wrapMsg model
            , FormActions.view
                (Routing.Questionnaires Questionnaires.Routing.Index)
                (ActionResult.ButtonConfig "Save" model.savingQuestionnaire (wrapMsg <| FormMsg Form.Submit) False)
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
                    FormGroup.select packageOptions model.form "packageId"

        accessibilitySelect =
            if appState.config.questionnaireAccessibilityEnabled then
                FormGroup.richRadioGroup QuestionnaireAccessibility.formOptions model.form "accessibility" "Accessibility"

            else
                emptyNode

        formHtml =
            div []
                [ FormGroup.input model.form "name" "Name"
                , parentInput "Knowledge Model"
                , accessibilitySelect
                ]
    in
    formHtml


tagsView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
tagsView wrapMsg model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag >> wrapMsg
            , removeMsg = RemoveTag >> wrapMsg
            }
    in
    Tag.selection tagListConfig model.knowledgeModelPreview


createOption : Package -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ Version.toString package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
