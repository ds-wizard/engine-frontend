module KMEditor.Editor.View.Tree exposing (treeView)

import Common.Html exposing (emptyNode, fa)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (getQuestionTitle, getReferenceVisibleName, isQuestionList, isQuestionOptions)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Msgs exposing (Msg(..))


treeView : String -> Dict String Editor -> String -> Html Msg
treeView activeUuid editors kmUuid =
    div [ class "diff-tree" ]
        [ ul [] [ treeNodeEditor activeUuid editors kmUuid ] ]


treeNodeEditor : String -> Dict String Editor -> String -> Html Msg
treeNodeEditor activeUuid editors editorUuid =
    case Dict.get editorUuid editors of
        Just (KMEditor data) ->
            treeNodeKM activeUuid editors data

        Just (TagEditor data) ->
            treeNodeTag activeUuid editors data

        Just (ChapterEditor data) ->
            treeNodeChapter activeUuid editors data

        Just (QuestionEditor data) ->
            treeNodeQuestion activeUuid editors data

        Just (AnswerEditor data) ->
            treeNodeAnswer activeUuid editors data

        Just (ReferenceEditor data) ->
            treeNodeReference activeUuid editors data

        Just (ExpertEditor data) ->
            treeNodeExpert activeUuid editors data

        _ ->
            emptyNode


treeNodeKM : String -> Dict String Editor -> KMEditorData -> Html Msg
treeNodeKM activeUuid editors editorData =
    let
        chapters =
            editorData.chapters.list ++ editorData.chapters.deleted

        tags =
            editorData.tags.list ++ editorData.tags.deleted

        config =
            { editorData = editorData
            , icon = "database"
            , label = editorData.knowledgeModel.name
            , children = chapters ++ tags
            }
    in
    treeNode config activeUuid editors


treeNodeTag : String -> Dict String Editor -> TagEditorData -> Html Msg
treeNodeTag activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = "tag"
            , label = editorData.tag.name
            , children = []
            }
    in
    treeNode config activeUuid editors


treeNodeChapter : String -> Dict String Editor -> ChapterEditorData -> Html Msg
treeNodeChapter activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = "book"
            , label = editorData.chapter.title
            , children = editorData.questions.list ++ editorData.questions.deleted
            }
    in
    treeNode config activeUuid editors


treeNodeQuestion : String -> Dict String Editor -> QuestionEditorData -> Html Msg
treeNodeQuestion activeUuid editors editorData =
    let
        itemQuestions =
            if isQuestionList editorData.question then
                editorData.itemQuestions.list ++ editorData.itemQuestions.deleted

            else
                []

        answers =
            if isQuestionOptions editorData.question then
                editorData.answers.list ++ editorData.answers.deleted

            else
                []

        references =
            editorData.references.list ++ editorData.references.deleted

        experts =
            editorData.experts.list ++ editorData.experts.deleted

        config =
            { editorData = editorData
            , icon = "comment-o"
            , label = getQuestionTitle editorData.question
            , children = itemQuestions ++ answers ++ references ++ experts
            }
    in
    treeNode config activeUuid editors


treeNodeAnswer : String -> Dict String Editor -> AnswerEditorData -> Html Msg
treeNodeAnswer activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = "check-square-o"
            , label = editorData.answer.label
            , children = editorData.followUps.list
            }
    in
    treeNode config activeUuid editors


treeNodeReference : String -> Dict String Editor -> ReferenceEditorData -> Html Msg
treeNodeReference activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = "bookmark-o"
            , label = getReferenceVisibleName editorData.reference
            , children = []
            }
    in
    treeNode config activeUuid editors


treeNodeExpert : String -> Dict String Editor -> ExpertEditorData -> Html Msg
treeNodeExpert activeUuid editors editorData =
    let
        config =
            { editorData = editorData
            , icon = "user-o"
            , label = editorData.expert.name
            , children = []
            }
    in
    treeNode config activeUuid editors


type alias TreeNodeConfig a e o =
    { editorData : EditorLike a e o
    , icon : String
    , label : String
    , children : List String
    }


treeNode : TreeNodeConfig a e o -> String -> Dict String Editor -> Html Msg
treeNode config activeUuid editors =
    let
        caret =
            if List.length config.children > 0 && config.editorData.editorState /= Deleted then
                treeNodeCaret (ToggleOpen config.editorData.uuid) config.editorData.treeOpen

            else
                emptyNode

        children =
            if config.editorData.treeOpen then
                ul [] (List.map (treeNodeEditor activeUuid editors) config.children)

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
            [ fa config.icon
            , span [] [ text config.label ]
            ]
        , children
        ]


treeNodeCaret : Msg -> Bool -> Html Msg
treeNodeCaret toggleMsg open =
    a [ onClick toggleMsg, class "caret" ]
        [ i [ class "fa", classList [ ( "fa-caret-right", not open ), ( "fa-caret-down", open ) ] ] []
        ]
