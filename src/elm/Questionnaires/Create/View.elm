module Questionnaires.Create.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.View.Flash as Flash
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Common.View.Tag as Tag
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Msgs
import Questionnaires.Create.Models exposing (Model, QuestionnaireCreateForm)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Routing
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (content wrapMsg model) model.packages


content : (Msg -> Msgs.Msg) -> Model -> List PackageDetail -> Html Msgs.Msg
content wrapMsg model packages =
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header "Create Questionnaire" []
        , div []
            [ FormResult.view model.savingQuestionnaire
            , formView model.form packages |> Html.map (wrapMsg << FormMsg)
            , tagsView wrapMsg model
            , FormActions.view (Routing.Questionnaires Questionnaires.Routing.Index) ( "Save", model.savingQuestionnaire, wrapMsg <| FormMsg Form.Submit )
            ]
        ]


formView : Form CustomFormError QuestionnaireCreateForm -> List PackageDetail -> Html Form.Msg
formView form packages =
    let
        packageOptions =
            ( "", "--" ) :: List.map createOption packages

        formHtml =
            div []
                [ FormGroup.input form "name" "Name"
                , FormGroup.select packageOptions form "packageId" "Knowledge Model"
                , FormGroup.toggle form "private" "Private"
                , FormExtra.text "If the questionnaire is private, it is visible only to you. Otherwise, it is visible to all users."
                ]
    in
    formHtml


tagsView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
tagsView wrapMsg model =
    let
        tagsContent =
            case model.knowledgeModelPreview of
                Unset ->
                    div [ class "alert alert-light" ]
                        [ i [] [ text "Select the knowledge model first" ] ]

                Loading ->
                    Flash.loader

                Error err ->
                    Flash.error err

                Success knowledgeModel ->
                    let
                        tagListConfig =
                            { selected = model.selectedTags
                            , addMsg = AddTag >> wrapMsg
                            , removeMsg = RemoveTag >> wrapMsg
                            }

                        extraText =
                            if List.length knowledgeModel.tags > 0 then
                                FormExtra.text "You can filter questions in the questionnaire by tags. If no tags are selected, all questions will be used."

                            else
                                emptyNode
                    in
                    div []
                        [ Tag.list tagListConfig knowledgeModel.tags
                        , extraText
                        ]
    in
    div [ class "form-group form-group-tags" ]
        [ label [] [ text "Tags" ]
        , div [] [ tagsContent ]
        ]


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
