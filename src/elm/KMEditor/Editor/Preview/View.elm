module KMEditor.Editor.Preview.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Locale exposing (l, lgx, lx)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Tag as Tag
import Html exposing (Html, a, div, strong)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModels
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Editor.Preview.Models exposing (Model)
import KMEditor.Editor.Preview.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.Preview.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KMEditor.Editor.Preview.View"


view : AppState -> List Level -> Model -> Html Msg
view appState levels model =
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
                , createRenderer = defaultQuestionnaireRenderer appState
                }
                appState
                model.questionnaireModel
                |> Html.map QuestionnaireMsg
    in
    div [ class "col KMEditor__Editor__Preview" ]
        [ tagSelection appState model.tags model.questionnaireModel
        , questionnaire
        ]


tagSelection : AppState -> List String -> Common.Questionnaire.Models.Model -> Html Msg
tagSelection appState selected questionnaireModel =
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
                [ strong [] [ lgx "tags" appState ]
                , a [ onClick SelectAllTags ] [ lx_ "tagSelection.all" appState ]
                , a [ onClick SelectNoneTags ] [ lx_ "tagSelection.none" appState ]
                ]
            , Tag.list appState tagListConfig tags
            ]

    else
        emptyNode
