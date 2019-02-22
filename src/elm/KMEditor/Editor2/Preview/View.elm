module KMEditor.Editor2.Preview.View exposing (view)

import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Tag as Tag
import Html exposing (Html, a, div, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Level)
import KMEditor.Editor2.Preview.Models exposing (Model)
import KMEditor.Editor2.Preview.Msgs exposing (Msg(..))
import Msgs


view : (Msg -> Msgs.Msg) -> List Level -> Model -> Html Msgs.Msg
view wrapMsg levels model =
    let
        questionnaire =
            viewQuestionnaire
                { showExtraActions = False
                , showExtraNavigation = False
                , levels = Just levels
                }
                model.questionnaireModel
                |> Html.map (QuestionnaireMsg >> wrapMsg)
    in
    div [ class "col KMEditor__Editor__Preview" ]
        [ tagSelection model.tags model.questionnaireModel |> Html.map wrapMsg
        , questionnaire
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
