module Wizard.KMEditor.Editor.KMEditor.View.Tree exposing (TreeNodeContext, treeView)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.KnowledgeModel.Reference as Reference
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (isListQuestionForm, isMultiChoiceQuestionForm, isOptionsQuestionForm)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (Msg(..))


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.KMEditor.View.Tree"


type alias TreeNodeContext =
    { activeUuid : String
    , editors : Dict String Editor
    , kmName : String
    }


treeView : AppState -> TreeNodeContext -> String -> Html Msg
treeView appState ctx kmUuid =
    div [ class "diff-tree" ]
        [ div [ class "inner" ]
            [ div [ class "actions" ]
                [ a [ onClick TreeExpandAll ]
                    [ faSet "kmEditor.expandAll" appState
                    , lx_ "expandAll" appState
                    ]
                , a [ onClick TreeCollapseAll ]
                    [ faSet "kmEditor.collapseAll" appState
                    , lx_ "collapseAll" appState
                    ]
                ]
            , ul [] [ treeNodeEditor appState ctx kmUuid ]
            ]
        ]


treeNodeEditor : AppState -> TreeNodeContext -> String -> Html Msg
treeNodeEditor appState ctx editorUuid =
    case Dict.get editorUuid ctx.editors of
        Just (KMEditor data) ->
            treeNodeKM appState ctx data

        Just (MetricEditor data) ->
            treeNodeMetric appState ctx data

        Just (PhaseEditor data) ->
            treeNodePhase appState ctx data

        Just (TagEditor data) ->
            treeNodeTag appState ctx data

        Just (IntegrationEditor data) ->
            treeNodeIntegration appState ctx data

        Just (ChapterEditor data) ->
            treeNodeChapter appState ctx data

        Just (QuestionEditor data) ->
            treeNodeQuestion appState ctx data

        Just (AnswerEditor data) ->
            treeNodeAnswer appState ctx data

        Just (ChoiceEditor data) ->
            treeNodeChoice appState ctx data

        Just (ReferenceEditor data) ->
            treeNodeReference appState ctx data

        Just (ExpertEditor data) ->
            treeNodeExpert appState ctx data

        _ ->
            emptyNode


treeNodeKM : AppState -> TreeNodeContext -> KMEditorData -> Html Msg
treeNodeKM appState ctx editorData =
    let
        chapters =
            editorData.chapters.list ++ editorData.chapters.deleted

        metrics =
            editorData.metrics.list ++ editorData.metrics.deleted

        phases =
            editorData.phases.list ++ editorData.phases.deleted

        tags =
            editorData.tags.list ++ editorData.tags.deleted

        integrations =
            editorData.integrations.list ++ editorData.integrations.deleted

        config =
            { editorData = editorData
            , icon = faSet "km.knowledgeModel" appState
            , label = ctx.kmName
            , children = chapters ++ metrics ++ phases ++ tags ++ integrations
            }
    in
    treeNode appState config ctx


treeNodeMetric : AppState -> TreeNodeContext -> MetricEditorData -> Html Msg
treeNodeMetric appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.metric" appState
            , label = editorData.metric.title
            , children = []
            }
    in
    treeNode appState config ctx


treeNodePhase : AppState -> TreeNodeContext -> PhaseEditorData -> Html Msg
treeNodePhase appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.phase" appState
            , label = editorData.phase.title
            , children = []
            }
    in
    treeNode appState config ctx


treeNodeTag : AppState -> TreeNodeContext -> TagEditorData -> Html Msg
treeNodeTag appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.tag" appState
            , label = editorData.tag.name
            , children = []
            }
    in
    treeNode appState config ctx


treeNodeIntegration : AppState -> TreeNodeContext -> IntegrationEditorData -> Html Msg
treeNodeIntegration appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.integration" appState
            , label = editorData.integration.name
            , children = []
            }
    in
    treeNode appState config ctx


treeNodeChapter : AppState -> TreeNodeContext -> ChapterEditorData -> Html Msg
treeNodeChapter appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.chapter" appState
            , label = editorData.chapter.title
            , children = editorData.questions.list ++ editorData.questions.deleted
            }
    in
    treeNode appState config ctx


treeNodeQuestion : AppState -> TreeNodeContext -> QuestionEditorData -> Html Msg
treeNodeQuestion appState ctx editorData =
    let
        itemTemplateQuestions =
            if isListQuestionForm editorData.form then
                editorData.itemTemplateQuestions.list ++ editorData.itemTemplateQuestions.deleted

            else
                []

        answers =
            if isOptionsQuestionForm editorData.form then
                editorData.answers.list ++ editorData.answers.deleted

            else
                []

        choices =
            if isMultiChoiceQuestionForm editorData.form then
                editorData.choices.list ++ editorData.choices.deleted

            else
                []

        references =
            editorData.references.list ++ editorData.references.deleted

        experts =
            editorData.experts.list ++ editorData.experts.deleted

        config =
            { editorData = editorData
            , icon = faSet "km.question" appState
            , label = Question.getTitle editorData.question
            , children = itemTemplateQuestions ++ answers ++ choices ++ references ++ experts
            }
    in
    treeNode appState config ctx


treeNodeAnswer : AppState -> TreeNodeContext -> AnswerEditorData -> Html Msg
treeNodeAnswer appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.answer" appState
            , label = editorData.answer.label
            , children = editorData.followUps.list
            }
    in
    treeNode appState config ctx


treeNodeChoice : AppState -> TreeNodeContext -> ChoiceEditorData -> Html Msg
treeNodeChoice appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.choice" appState
            , label = editorData.choice.label
            , children = []
            }
    in
    treeNode appState config ctx


treeNodeReference : AppState -> TreeNodeContext -> ReferenceEditorData -> Html Msg
treeNodeReference appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.reference" appState
            , label = Reference.getVisibleName editorData.reference
            , children = []
            }
    in
    treeNode appState config ctx


treeNodeExpert : AppState -> TreeNodeContext -> ExpertEditorData -> Html Msg
treeNodeExpert appState ctx editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.expert" appState
            , label = editorData.expert.name
            , children = []
            }
    in
    treeNode appState config ctx


type alias TreeNodeConfig a e o =
    { editorData : EditorLike a e o
    , icon : Html Msg
    , label : String
    , children : List String
    }


treeNode : AppState -> TreeNodeConfig a e o -> TreeNodeContext -> Html Msg
treeNode appState config ctx =
    let
        caret =
            if List.length config.children > 0 && config.editorData.editorState /= Deleted then
                treeNodeCaret appState (ToggleOpen config.editorData.uuid) config.editorData.treeOpen

            else
                emptyNode

        children =
            if config.editorData.treeOpen then
                ul [] (List.map (treeNodeEditor appState ctx) config.children)

            else
                emptyNode

        link =
            if config.editorData.editorState == Deleted then
                a []

            else
                a [ onClick <| SetActiveEditor config.editorData.uuid ]
    in
    li
        [ classList
            [ ( "active", config.editorData.uuid == ctx.activeUuid )
            , ( "state-edited", config.editorData.editorState == Edited )
            , ( "state-deleted", config.editorData.editorState == Deleted )
            , ( "state-added", config.editorData.editorState == Added || config.editorData.editorState == AddedEdited )
            ]
        ]
        [ caret
        , link
            [ config.icon
            , span [] [ text config.label ]
            ]
        , children
        ]


treeNodeCaret : AppState -> Msg -> Bool -> Html Msg
treeNodeCaret appState toggleMsg open =
    a [ onClick toggleMsg, class "caret" ]
        [ i
            [ classList
                [ ( faKeyClass "kmEditor.treeClosed" appState, not open )
                , ( faKeyClass "kmEditor.treeOpened" appState, open )
                ]
            ]
            []
        ]
