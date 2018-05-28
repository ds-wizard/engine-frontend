module KMEditor.View exposing (diffTreeView)

import Common.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (class)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import List.Extra as List


diffTreeView : KnowledgeModel -> List Event -> Html msg
diffTreeView km events =
    diffTreeNodeKnowledgeModel (List.reverse events) km


diffTreeNodeKnowledgeModel : List Event -> KnowledgeModel -> Html msg
diffTreeNodeKnowledgeModel events km =
    let
        newChapters =
            newChildren isAddChapter diffTreeNodeNewChapter events km
    in
    div [ class "diff-tree" ]
        [ ul []
            (List.map (diffTreeNodeChapter events) km.chapters ++ newChapters)
        ]


diffTreeNodeChapter : List Event -> Chapter -> Html msg
diffTreeNodeChapter events chapter =
    let
        divClass =
            getClass isDeleteChapter isEditChapter events chapter

        newQuestions =
            newChildren isAddQuestion diffTreeNodeNewQuestion events chapter.uuid
    in
    li [ class (divClass ++ " chapter") ]
        [ strong [] [ text chapter.title ]
        , ul []
            (List.map (diffTreeNodeQuestion events) chapter.questions ++ newQuestions)
        ]


diffTreeNodeNewChapter : List Event -> Event -> Html msg
diffTreeNodeNewChapter events event =
    case event of
        AddChapterEvent eventData _ ->
            let
                dummyChapter =
                    newChapter eventData.chapterUuid
            in
            if List.any (isDeleteChapter dummyChapter) events then
                emptyNode
            else
                List.find (isEditChapter dummyChapter) events
                    |> Maybe.andThen getEventEntityVisibleName
                    |> Maybe.withDefault eventData.title
                    |> chapterNewNode

        _ ->
            emptyNode


chapterNewNode : String -> Html msg
chapterNewNode title =
    li [ class "ins chapter" ]
        [ strong [] [ text title ] ]


diffTreeNodeQuestion : List Event -> Question -> Html msg
diffTreeNodeQuestion events question =
    let
        divClass =
            getClass isDeleteQuestion isEditQuestion events question

        newAnswers =
            newChildren isAddAnswer diffTreeNodeNewAnswer events question

        newReferences =
            newChildren isAddReference diffTreeNodeNewReference events question

        newExperts =
            newChildren isAddExpert diffTreeNodeNewExpert events question
    in
    li [ class (divClass ++ " question") ]
        [ span []
            [ i [ class "fa fa-comment-o" ] []
            , text question.title
            ]
        , ul []
            (List.map (diffTreeNodeAnswer events) (question.answers |> Maybe.withDefault []) ++ newAnswers)
        , ul []
            (List.map (diffTreeNodeReference events) question.references ++ newReferences)
        , ul []
            (List.map (diffTreeNodeExpert events) question.experts ++ newExperts)
        ]


diffTreeNodeNewQuestion : List Event -> Event -> Html msg
diffTreeNodeNewQuestion events event =
    let
        getNode { questionUuid, title } =
            let
                dummyQuestion =
                    newQuestion questionUuid
            in
            if List.any (isDeleteQuestion dummyQuestion) events then
                emptyNode
            else
                List.find (isEditQuestion dummyQuestion) events
                    |> Maybe.andThen getEventEntityVisibleName
                    |> Maybe.withDefault title
                    |> diffTreeNewNode "question" "fa-comment-o"
    in
    case event of
        AddQuestionEvent eventData _ ->
            getNode eventData

        _ ->
            emptyNode


diffTreeNodeAnswer : List Event -> Answer -> Html msg
diffTreeNodeAnswer events answer =
    let
        divClass =
            getClass isDeleteAnswer isEditAnswer events answer

        newQuestions =
            newChildren isAddQuestion diffTreeNodeNewQuestion events answer.uuid
    in
    li [ class (divClass ++ " answer") ]
        [ span []
            [ i [ class "fa fa-check-square-o" ] []
            , text answer.label
            ]
        , ul []
            (List.map (diffTreeNodeQuestion events) (getFollowUpQuestions answer) ++ newQuestions)
        ]


diffTreeNodeNewAnswer : List Event -> Event -> Html msg
diffTreeNodeNewAnswer events event =
    case event of
        AddAnswerEvent eventData _ ->
            let
                dummyAnswer =
                    newAnswer eventData.answerUuid
            in
            if List.any (isDeleteAnswer dummyAnswer) events then
                emptyNode
            else
                List.find (isEditAnswer dummyAnswer) events
                    |> Maybe.andThen getEventEntityVisibleName
                    |> Maybe.withDefault eventData.label
                    |> diffTreeNewNode "answer" "fa-check-square-o"

        _ ->
            emptyNode


diffTreeNodeReference : List Event -> Reference -> Html msg
diffTreeNodeReference events reference =
    let
        divClass =
            getClass isDeleteReference isEditReference events reference
    in
    li [ class (divClass ++ " reference") ]
        [ span []
            [ i [ class "fa fa-book" ] []
            , text reference.chapter
            ]
        ]


diffTreeNodeNewReference : List Event -> Event -> Html msg
diffTreeNodeNewReference events event =
    case event of
        AddReferenceEvent eventData _ ->
            let
                dummyReference =
                    newReference eventData.referenceUuid
            in
            if List.any (isDeleteReference dummyReference) events then
                emptyNode
            else
                List.find (isEditReference (newReference eventData.referenceUuid)) events
                    |> Maybe.andThen getEventEntityVisibleName
                    |> Maybe.withDefault eventData.chapter
                    |> diffTreeNewNode "reference" "fa-book"

        _ ->
            emptyNode


diffTreeNodeExpert : List Event -> Expert -> Html msg
diffTreeNodeExpert events expert =
    let
        divClass =
            getClass isDeleteExpert isEditExpert events expert
    in
    li [ class (divClass ++ " expert") ]
        [ span []
            [ i [ class "fa fa-user-o" ] []
            , text expert.name
            ]
        ]


diffTreeNodeNewExpert : List Event -> Event -> Html msg
diffTreeNodeNewExpert events event =
    case event of
        AddExpertEvent eventData _ ->
            let
                dummyExpert =
                    newExpert eventData.expertUuid
            in
            if List.any (isDeleteExpert dummyExpert) events then
                emptyNode
            else
                List.find (isEditExpert (newExpert eventData.expertUuid)) events
                    |> Maybe.andThen getEventEntityVisibleName
                    |> Maybe.withDefault eventData.name
                    |> diffTreeNewNode "expert" "fa-user-o"

        _ ->
            emptyNode


diffTreeNewNode : String -> String -> String -> Html msg
diffTreeNewNode cssClass icon title =
    li [ class <| "ins " ++ cssClass ]
        [ span []
            [ i [ class <| "fa " ++ icon ] []
            , text title
            ]
        ]


getClass : (a -> Event -> Bool) -> (a -> Event -> Bool) -> List Event -> a -> String
getClass isDelete isEdit events entity =
    if List.any (isDelete entity) events then
        "del"
    else if List.any (isEdit entity) events then
        "edited"
    else
        ""


newChildren : (a -> Event -> Bool) -> (List Event -> Event -> Html msg) -> List Event -> a -> List (Html msg)
newChildren isAdd newNode events parent =
    List.filter (isAdd parent) events
        |> List.map (newNode events)
