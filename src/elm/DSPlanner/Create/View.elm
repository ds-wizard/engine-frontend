module DSPlanner.Create.View exposing (content, createOption, formView, view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode, inlineLoader)
import Common.View.FormGroup as FormGroup
import Common.View.Forms exposing (errorView, formActions, formResultView, formText)
import Common.View.Page as Page
import Common.View.Tags exposing (tagList)
import DSPlanner.Create.Models exposing (Model, QuestionnaireCreateForm)
import DSPlanner.Create.Msgs exposing (Msg(..))
import DSPlanner.Routing
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import KMPackages.Common.Models exposing (PackageDetail)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "DSPlanner__Create" ]
        [ Page.header "Create Questionnaire" []
        , Page.actionResultView (content wrapMsg model) model.packages
        ]


content : (Msg -> Msgs.Msg) -> Model -> List PackageDetail -> Html Msgs.Msg
content wrapMsg model packages =
    div []
        [ formResultView model.savingQuestionnaire
        , formView model.form packages |> Html.map (wrapMsg << FormMsg)
        , tagsView wrapMsg model
        , formActions (Routing.DSPlanner DSPlanner.Routing.Index) ( "Save", model.savingQuestionnaire, wrapMsg <| FormMsg Form.Submit )
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
                , formText "If the questionnaire is private, it is visible only to you. Otherwise, it is visible to all users."
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
                    inlineLoader

                Error err ->
                    errorView err

                Success knowledgeModel ->
                    let
                        tagListConfig =
                            { selected = model.selectedTags
                            , addMsg = AddTag >> wrapMsg
                            , removeMsg = RemoveTag >> wrapMsg
                            }

                        extraText =
                            if List.length knowledgeModel.tags > 0 then
                                formText "You can filter questions in the questionnaire by selecting only some tags. If no tags are selected, all questions will be used."

                            else
                                emptyNode
                    in
                    div []
                        [ tagList tagListConfig knowledgeModel.tags
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
