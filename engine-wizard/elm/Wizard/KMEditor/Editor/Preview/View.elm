module Wizard.KMEditor.Editor.Preview.View exposing (view)

import Html exposing (Html, a, div, strong)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel as KnowledgeModels exposing (KnowledgeModel)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lgx, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Editor.Preview.Models exposing (Model)
import Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.Preview.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.Preview.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        questionnaire =
            Html.map QuestionnaireMsg <|
                Questionnaire.view appState
                    { features =
                        { feedbackEnabled = False
                        , todosEnabled = False
                        , readonly = False
                        }
                    , renderer = DefaultQuestionnaireRenderer.create appState model.knowledgeModel model.levels model.metrics
                    }
                    { metrics = model.metrics
                    , levels = model.levels
                    }
                    model.questionnaireModel
    in
    div [ class "col KMEditor__Editor__Preview" ]
        [ tagSelection appState model.tags model.knowledgeModel
        , questionnaire
        ]


tagSelection : AppState -> List String -> KnowledgeModel -> Html Msg
tagSelection appState selected knowledgeModel =
    if List.length knowledgeModel.tagUuids > 0 then
        let
            tags =
                KnowledgeModels.getTags knowledgeModel

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
