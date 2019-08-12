module KMEditor.Editor.Preview.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Tag as Tag
import Html exposing (Html, a, div, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModels
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Editor.Preview.Models exposing (Model)
import KMEditor.Editor.Preview.Msgs exposing (Msg(..))
import Msgs


view : (Msg -> Msgs.Msg) -> AppState -> List Level -> Model -> Html Msgs.Msg
view wrapMsg appState levels model =
    let
        questionnaire =
            viewQuestionnaire
                { features = []
                , levels =
                    if appState.config.levelsEnabled then
                        Just levels

                    else
                        Nothing
                , getExtraQuestionClass = always Nothing
                , forceDisabled = False
                , createRenderer = defaultQuestionnaireRenderer
                }
                appState
                model.questionnaireModel
                |> Html.map (QuestionnaireMsg >> wrapMsg)
    in
    div [ class "col KMEditor__Editor__Preview" ]
        [ tagSelection model.tags model.questionnaireModel |> Html.map wrapMsg
        , questionnaire
        ]


tagSelection : List String -> Common.Questionnaire.Models.Model -> Html Msg
tagSelection selected questionnaireModel =
    if List.length questionnaireModel.questionnaire.knowledgeModel.tagUuids > 0 then
        let
            tags =
                KnowledgeModels.getTags questionnaireModel.questionnaire.knowledgeModel

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
            , Tag.list tagListConfig tags
            ]

    else
        emptyNode
