module KMEditor.Common.View exposing (diffTreeView)

import Common.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (class)
import KMEditor.Common.Events.Event as Event exposing (Event(..))
import KMEditor.Common.KnowledgeModel.Answer as Answer exposing (Answer)
import KMEditor.Common.KnowledgeModel.Chapter as Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.Expert as Expert exposing (Expert)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question)
import KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference)
import KMEditor.Common.KnowledgeModel.Tag as Tag exposing (Tag)
import List.Extra as List


diffTreeView : KnowledgeModel -> List Event -> Html msg
diffTreeView km events =
    diffTreeNodeKnowledgeModel (List.reverse events) km


diffTreeNodeKnowledgeModel : List Event -> KnowledgeModel -> Html msg
diffTreeNodeKnowledgeModel events km =
    let
        newChapters =
            newChildren Event.isAddChapter diffTreeNodeNewChapter events km

        newTags =
            newChildren Event.isAddTag diffTreeNodeNewTag events km

        chapters =
            KnowledgeModel.getChapters km

        tags =
            KnowledgeModel.getTags km
    in
    div [ class "diff-tree" ]
        [ ul []
            (List.map (diffTreeNodeChapter events km) chapters ++ newChapters)
        , ul []
            (List.map (diffTreeNodeTag events) tags ++ newTags)
        ]


diffTreeNodeChapter : List Event -> KnowledgeModel -> Chapter -> Html msg
diffTreeNodeChapter events km chapter =
    let
        divClass =
            getClass Event.isDeleteChapter Event.isEditChapter events chapter

        newQuestions =
            newChildren Event.isAddQuestion diffTreeNodeNewQuestion events chapter.uuid

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km
    in
    li [ class (divClass ++ " chapter") ]
        [ span []
            [ i [ class "fa fa-book" ] []
            , strong [] [ text chapter.title ]
            ]
        , ul []
            (List.map (diffTreeNodeQuestion events km) questions ++ newQuestions)
        ]


diffTreeNodeNewChapter : List Event -> Event -> Html msg
diffTreeNodeNewChapter events event =
    case event of
        AddChapterEvent eventData commonData ->
            let
                dummyChapter =
                    Chapter.new commonData.entityUuid
            in
            if List.any (Event.isDeleteChapter dummyChapter) events then
                emptyNode

            else
                List.find (Event.isEditChapter dummyChapter) events
                    |> Maybe.andThen Event.getEntityVisibleName
                    |> Maybe.withDefault eventData.title
                    |> chapterNewNode

        _ ->
            emptyNode


chapterNewNode : String -> Html msg
chapterNewNode title =
    li [ class "ins chapter" ]
        [ span []
            [ i [ class "fa fa-book" ] []
            , strong [] [ text title ]
            ]
        ]


diffTreeNodeTag : List Event -> Tag -> Html msg
diffTreeNodeTag events tag =
    let
        divClass =
            getClass Event.isDeleteTag Event.isEditTag events tag
    in
    li [ class (divClass ++ " tag") ]
        [ span []
            [ i [ class "fa fa-tag" ] []
            , text tag.name
            ]
        ]


diffTreeNodeNewTag : List Event -> Event -> Html msg
diffTreeNodeNewTag events event =
    case event of
        AddTagEvent eventData commonData ->
            let
                dummyTag =
                    Tag.new commonData.entityUuid
            in
            if List.any (Event.isDeleteTag dummyTag) events then
                emptyNode

            else
                List.find (Event.isEditTag dummyTag) events
                    |> Maybe.andThen Event.getEntityVisibleName
                    |> Maybe.withDefault eventData.name
                    |> diffTreeNewNode "tag" "fa-tag"

        _ ->
            emptyNode


diffTreeNodeQuestion : List Event -> KnowledgeModel -> Question -> Html msg
diffTreeNodeQuestion events km question =
    let
        questionUuid =
            Question.getUuid question

        divClass =
            getClass Event.isDeleteQuestion Event.isEditQuestion events question

        newAnswers =
            newChildren Event.isAddAnswer diffTreeNodeNewAnswer events question

        newItemQuestions =
            newChildren Event.isAddQuestion diffTreeNodeNewQuestion events questionUuid

        newReferences =
            newChildren Event.isAddReference diffTreeNodeNewReference events question

        newExperts =
            newChildren Event.isAddExpert diffTreeNodeNewExpert events question

        answers =
            KnowledgeModel.getQuestionAnswers questionUuid km

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        experts =
            KnowledgeModel.getQuestionExperts questionUuid km
    in
    li [ class (divClass ++ " question") ]
        [ span []
            [ i [ class "fa fa-comment-o" ] []
            , text (Question.getTitle question)
            ]
        , ul []
            (List.map (diffTreeNodeAnswer events km) answers ++ newAnswers)
        , ul []
            (List.map (diffTreeNodeQuestion events km) itemTemplateQuestions ++ newItemQuestions)
        , ul []
            (List.map (diffTreeNodeReference events) references ++ newReferences)
        , ul []
            (List.map (diffTreeNodeExpert events) experts ++ newExperts)
        ]


diffTreeNodeNewQuestion : List Event -> Event -> Html msg
diffTreeNodeNewQuestion events event =
    let
        getNode questionUuid title =
            let
                dummyQuestion =
                    Question.new questionUuid
            in
            if List.any (Event.isDeleteQuestion dummyQuestion) events then
                emptyNode

            else
                List.find (Event.isEditQuestion dummyQuestion) events
                    |> Maybe.andThen Event.getEntityVisibleName
                    |> Maybe.withDefault title
                    |> diffTreeNewNode "question" "fa-comment-o"
    in
    case event of
        AddQuestionEvent _ commonData ->
            getNode commonData.entityUuid (Event.getEntityVisibleName event |> Maybe.withDefault "")

        _ ->
            emptyNode


diffTreeNodeAnswer : List Event -> KnowledgeModel -> Answer -> Html msg
diffTreeNodeAnswer events km answer =
    let
        divClass =
            getClass Event.isDeleteAnswer Event.isEditAnswer events answer

        newQuestions =
            newChildren Event.isAddQuestion diffTreeNodeNewQuestion events answer.uuid

        followUpQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km
    in
    li [ class (divClass ++ " answer") ]
        [ span []
            [ i [ class "fa fa-check-square-o" ] []
            , text answer.label
            ]
        , ul []
            (List.map (diffTreeNodeQuestion events km) followUpQuestions ++ newQuestions)
        ]


diffTreeNodeNewAnswer : List Event -> Event -> Html msg
diffTreeNodeNewAnswer events event =
    case event of
        AddAnswerEvent eventData commonData ->
            let
                dummyAnswer =
                    Answer.new commonData.entityUuid
            in
            if List.any (Event.isDeleteAnswer dummyAnswer) events then
                emptyNode

            else
                List.find (Event.isEditAnswer dummyAnswer) events
                    |> Maybe.andThen Event.getEntityVisibleName
                    |> Maybe.withDefault eventData.label
                    |> diffTreeNewNode "answer" "fa-check-square-o"

        _ ->
            emptyNode


diffTreeNodeReference : List Event -> Reference -> Html msg
diffTreeNodeReference events reference =
    let
        divClass =
            getClass Event.isDeleteReference Event.isEditReference events reference
    in
    li [ class (divClass ++ " reference") ]
        [ span []
            [ i [ class "fa fa-bookmark-o" ] []
            , text <| Reference.getVisibleName reference
            ]
        ]


diffTreeNodeNewReference : List Event -> Event -> Html msg
diffTreeNodeNewReference events event =
    case event of
        AddReferenceEvent _ commonData ->
            let
                dummyReference =
                    Reference.new commonData.entityUuid
            in
            if List.any (Event.isDeleteReference dummyReference) events then
                emptyNode

            else
                List.find (Event.isEditReference dummyReference) events
                    |> Maybe.andThen Event.getEntityVisibleName
                    |> Maybe.withDefault "Reference"
                    |> diffTreeNewNode "reference" "fa-bookmark-o"

        _ ->
            emptyNode


diffTreeNodeExpert : List Event -> Expert -> Html msg
diffTreeNodeExpert events expert =
    let
        divClass =
            getClass Event.isDeleteExpert Event.isEditExpert events expert
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
        AddExpertEvent eventData commonData ->
            let
                dummyExpert =
                    Expert.new commonData.entityUuid
            in
            if List.any (Event.isDeleteExpert dummyExpert) events then
                emptyNode

            else
                List.find (Event.isEditExpert dummyExpert) events
                    |> Maybe.andThen Event.getEntityVisibleName
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
