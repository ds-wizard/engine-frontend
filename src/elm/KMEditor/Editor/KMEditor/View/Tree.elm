module KMEditor.Editor.KMEditor.View.Tree exposing (treeView)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa, faSet)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.Question as Question
import KMEditor.Common.KnowledgeModel.Reference as Reference
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (isListQuestionForm, isOptionsQuestionForm)
import KMEditor.Editor.KMEditor.Msgs exposing (Msg(..))


treeView : AppState -> String -> Dict String Editor -> String -> Html Msg
treeView appState activeUuid editors kmUuid =
    div [ class "diff-tree" ]
        [ ul [] [ treeNodeEditor appState activeUuid editors kmUuid ] ]


treeNodeEditor : AppState -> String -> Dict String Editor -> String -> Html Msg
treeNodeEditor appState activeUuid editors editorUuid =
    case Dict.get editorUuid editors of
        Just (KMEditor data) ->
            treeNodeKM appState activeUuid editors data

        Just (TagEditor data) ->
            treeNodeTag appState activeUuid editors data

        Just (IntegrationEditor data) ->
            treeNodeIntegration appState activeUuid editors data

        Just (ChapterEditor data) ->
            treeNodeChapter appState activeUuid editors data

        Just (QuestionEditor data) ->
            treeNodeQuestion appState activeUuid editors data

        Just (AnswerEditor data) ->
            treeNodeAnswer appState activeUuid editors data

        Just (ReferenceEditor data) ->
            treeNodeReference appState activeUuid editors data

        Just (ExpertEditor data) ->
            treeNodeExpert appState activeUuid editors data

        _ ->
            emptyNode


treeNodeKM : AppState -> String -> Dict String Editor -> KMEditorData -> Html Msg
treeNodeKM appState activeUuid editors editorData =
    let
        chapters =
            editorData.chapters.list ++ editorData.chapters.deleted

        tags =
            editorData.tags.list ++ editorData.tags.deleted

        integrations =
            editorData.integrations.list ++ editorData.integrations.deleted

        config =
            { editorData = editorData
            , icon = faSet "km.knowledgeModel" appState
            , label = editorData.knowledgeModel.name
            , children = chapters ++ tags ++ integrations
            }
    in
    treeNode appState config activeUuid editors


treeNodeTag : AppState -> String -> Dict String Editor -> TagEditorData -> Html Msg
treeNodeTag appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.tag" appState
            , label = editorData.tag.name
            , children = []
            }
    in
    treeNode appState config activeUuid editors


treeNodeIntegration : AppState -> String -> Dict String Editor -> IntegrationEditorData -> Html Msg
treeNodeIntegration appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.integration" appState
            , label = editorData.integration.name
            , children = []
            }
    in
    treeNode appState config activeUuid editors


treeNodeChapter : AppState -> String -> Dict String Editor -> ChapterEditorData -> Html Msg
treeNodeChapter appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.chapter" appState
            , label = editorData.chapter.title
            , children = editorData.questions.list ++ editorData.questions.deleted
            }
    in
    treeNode appState config activeUuid editors


treeNodeQuestion : AppState -> String -> Dict String Editor -> QuestionEditorData -> Html Msg
treeNodeQuestion appState activeUuid editors editorData =
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

        references =
            editorData.references.list ++ editorData.references.deleted

        experts =
            editorData.experts.list ++ editorData.experts.deleted

        config =
            { editorData = editorData
            , icon = faSet "km.question" appState
            , label = Question.getTitle editorData.question
            , children = itemTemplateQuestions ++ answers ++ references ++ experts
            }
    in
    treeNode appState config activeUuid editors


treeNodeAnswer : AppState -> String -> Dict String Editor -> AnswerEditorData -> Html Msg
treeNodeAnswer appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.answer" appState
            , label = editorData.answer.label
            , children = editorData.followUps.list
            }
    in
    treeNode appState config activeUuid editors


treeNodeReference : AppState -> String -> Dict String Editor -> ReferenceEditorData -> Html Msg
treeNodeReference appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.reference" appState
            , label = Reference.getVisibleName editorData.reference
            , children = []
            }
    in
    treeNode appState config activeUuid editors


treeNodeExpert : AppState -> String -> Dict String Editor -> ExpertEditorData -> Html Msg
treeNodeExpert appState activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = faSet "km.expert" appState
            , label = editorData.expert.name
            , children = []
            }
    in
    treeNode appState config activeUuid editors


type alias TreeNodeConfig a e o =
    { editorData : EditorLike a e o
    , icon : Html Msg
    , label : String
    , children : List String
    }


treeNode : AppState -> TreeNodeConfig a e o -> String -> Dict String Editor -> Html Msg
treeNode appState config activeUuid editors =
    let
        caret =
            if List.length config.children > 0 && config.editorData.editorState /= Deleted then
                treeNodeCaret (ToggleOpen config.editorData.uuid) config.editorData.treeOpen

            else
                emptyNode

        children =
            if config.editorData.treeOpen then
                ul [] (List.map (treeNodeEditor appState activeUuid editors) config.children)

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
            [ ( "active", config.editorData.uuid == activeUuid )
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


treeNodeCaret : Msg -> Bool -> Html Msg
treeNodeCaret toggleMsg open =
    a [ onClick toggleMsg, class "caret" ]
        [ i [ class "fa", classList [ ( "fa-caret-right", not open ), ( "fa-caret-down", open ) ] ] []
        ]
