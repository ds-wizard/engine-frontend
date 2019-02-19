module KMEditor.Preview.View exposing (view)

import ActionResult
import Common.Html exposing (emptyNode, linkTo)
import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Common.View.Tag as Tag
import Html exposing (Html, a, button, div, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Level)
import KMEditor.Preview.Models exposing (Model)
import KMEditor.Preview.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    ActionResult.combine model.questionnaireModel model.levels
        |> Page.actionResultView (content wrapMsg model)


content : (Msg -> Msgs.Msg) -> Model -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msgs.Msg
content wrapMsg model ( questionnaireModel, levels ) =
    let
        questionnaire =
            viewQuestionnaire
                { showExtraActions = False
                , showExtraNavigation = False
                , levels = Just levels
                }
                questionnaireModel
                |> Html.map (QuestionnaireMsg >> wrapMsg)
    in
    div [ class "col KMEditor__Preview" ]
        [ questionnaireHeader model.branchUuid
        , tagSelection model.tags questionnaireModel |> Html.map wrapMsg
        , questionnaire
        ]


questionnaireHeader : String -> Html Msgs.Msg
questionnaireHeader uuid =
    div [ class "questionnaire-header" ]
        [ div [ class "questionnaire-header-content" ]
            [ text "Questionnaire Preview"
            , div []
                [ linkTo (KMEditor <| EditorRoute uuid) [ class "btn btn-primary" ] [ text "Edit Knowledge Model" ] ]
            ]
        ]


tagSelection : List String -> Common.Questionnaire.Models.Model -> Html Msg
tagSelection selected questionnaireModel =
    let
        tagListConfig =
            { selected = selected
            , addMsg = AddTag
            , removeMsg = RemoveTag
            }
    in
    div [ class "tag-selection" ]
        [ div [ class "tag-selection-header" ]
            [ strong [] [ text "Tags" ]
            , a [ onClick SelectAllTags ] [ text "Select All" ]
            , a [ onClick SelectNoneTags ] [ text "Select None" ]
            ]
        , Tag.list tagListConfig questionnaireModel.questionnaire.knowledgeModel.tags
        ]
