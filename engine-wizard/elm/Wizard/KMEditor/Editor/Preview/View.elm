module Wizard.KMEditor.Editor.Preview.View exposing (view)

import Html exposing (Html, a, div, strong)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (lgx, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Editor.Preview.Models exposing (Model)
import Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.Preview.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , readonly = False
                    , toolbarEnabled = False
                    }
                , renderer = DefaultQuestionnaireRenderer.create appState model.knowledgeModel
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                { events = model.events
                }
                model.questionnaireModel
    in
    div [ class "col KMEditor__Editor__Preview", dataCy "km-editor_preview" ]
        [ tagSelection appState model.tags model.knowledgeModel
        , questionnaire
        ]


tagSelection : AppState -> List String -> KnowledgeModel -> Html Msg
tagSelection appState selected knowledgeModel =
    if List.length knowledgeModel.tagUuids > 0 then
        let
            tags =
                KnowledgeModel.getTags knowledgeModel

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
