module KMEditor.Common.View exposing (diffTreeView)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, faSet)
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


diffTreeView : AppState -> KnowledgeModel -> List Event -> Html msg
diffTreeView appState km events =
    diffTreeNodeKnowledgeModel appState (List.reverse events) km


diffTreeNodeKnowledgeModel : AppState -> List Event -> KnowledgeModel -> Html msg
diffTreeNodeKnowledgeModel appState events km =
    let
        newChapters =
            newChildren Event.isAddChapter (diffTreeNodeNewChapter appState) events km

        newTags =
            newChildren Event.isAddTag (diffTreeNodeNewTag appState) events km

        chapters =
            KnowledgeModel.getChapters km

        tags =
            KnowledgeModel.getTags km
    in
    div [ class "diff-tree" ]
        [ ul []
            (List.map (diffTreeNodeChapter appState events km) chapters ++ newChapters)
        , ul []
            (List.map (diffTreeNodeTag appState events) tags ++ newTags)
        ]


diffTreeNodeChapter : AppState -> List Event -> KnowledgeModel -> Chapter -> Html msg
diffTreeNodeChapter appState events km chapter =
    let
        divClass =
            getClass Event.isDeleteChapter Event.isEditChapter events chapter

        newQuestions =
            newChildren Event.isAddQuestion (diffTreeNodeNewQuestion appState) events chapter.uuid

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km
    in
    li [ class (divClass ++ " chapter") ]
        [ span []
            [ faSet "km.chapter" appState
            , strong [] [ text chapter.title ]
            ]
        , ul []
            (List.map (diffTreeNodeQuestion appState events km) questions ++ newQuestions)
        ]


diffTreeNodeNewChapter : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewChapter appState events event =
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
                    |> chapterNewNode appState

        _ ->
            emptyNode


chapterNewNode : AppState -> String -> Html msg
chapterNewNode appState title =
    li [ class "ins chapter" ]
        [ span []
            [ faSet "km.chapter" appState
            , strong [] [ text title ]
            ]
        ]


diffTreeNodeTag : AppState -> List Event -> Tag -> Html msg
diffTreeNodeTag appState events tag =
    let
        divClass =
            getClass Event.isDeleteTag Event.isEditTag events tag
    in
    li [ class (divClass ++ " tag") ]
        [ span []
            [ faSet "km.tag" appState
            , text tag.name
            ]
        ]


diffTreeNodeNewTag : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewTag appState events event =
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
                    |> diffTreeNewNode "tag" (faSet "km.tag" appState)

        _ ->
            emptyNode


diffTreeNodeQuestion : AppState -> List Event -> KnowledgeModel -> Question -> Html msg
diffTreeNodeQuestion appState events km question =
    let
        questionUuid =
            Question.getUuid question

        divClass =
            getClass Event.isDeleteQuestion Event.isEditQuestion events question

        newAnswers =
            newChildren Event.isAddAnswer (diffTreeNodeNewAnswer appState) events question

        newItemQuestions =
            newChildren Event.isAddQuestion (diffTreeNodeNewQuestion appState) events questionUuid

        newReferences =
            newChildren Event.isAddReference (diffTreeNodeNewReference appState) events question

        newExperts =
            newChildren Event.isAddExpert (diffTreeNodeNewExpert appState) events question

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
            [ faSet "km.question" appState
            , text (Question.getTitle question)
            ]
        , ul []
            (List.map (diffTreeNodeAnswer appState events km) answers ++ newAnswers)
        , ul []
            (List.map (diffTreeNodeQuestion appState events km) itemTemplateQuestions ++ newItemQuestions)
        , ul []
            (List.map (diffTreeNodeReference appState events) references ++ newReferences)
        , ul []
            (List.map (diffTreeNodeExpert appState events) experts ++ newExperts)
        ]


diffTreeNodeNewQuestion : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewQuestion appState events event =
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
                    |> diffTreeNewNode "question" (faSet "km.question" appState)
    in
    case event of
        AddQuestionEvent _ commonData ->
            getNode commonData.entityUuid (Event.getEntityVisibleName event |> Maybe.withDefault "")

        _ ->
            emptyNode


diffTreeNodeAnswer : AppState -> List Event -> KnowledgeModel -> Answer -> Html msg
diffTreeNodeAnswer appState events km answer =
    let
        divClass =
            getClass Event.isDeleteAnswer Event.isEditAnswer events answer

        newQuestions =
            newChildren Event.isAddQuestion (diffTreeNodeNewQuestion appState) events answer.uuid

        followUpQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km
    in
    li [ class (divClass ++ " answer") ]
        [ span []
            [ faSet "km.answer" appState
            , text answer.label
            ]
        , ul []
            (List.map (diffTreeNodeQuestion appState events km) followUpQuestions ++ newQuestions)
        ]


diffTreeNodeNewAnswer : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewAnswer appState events event =
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
                    |> diffTreeNewNode "answer" (faSet "km.answer" appState)

        _ ->
            emptyNode


diffTreeNodeReference : AppState -> List Event -> Reference -> Html msg
diffTreeNodeReference appState events reference =
    let
        divClass =
            getClass Event.isDeleteReference Event.isEditReference events reference
    in
    li [ class (divClass ++ " reference") ]
        [ span []
            [ faSet "km.reference" appState
            , text <| Reference.getVisibleName reference
            ]
        ]


diffTreeNodeNewReference : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewReference appState events event =
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
                    |> diffTreeNewNode "reference" (faSet "km.reference" appState)

        _ ->
            emptyNode


diffTreeNodeExpert : AppState -> List Event -> Expert -> Html msg
diffTreeNodeExpert appState events expert =
    let
        divClass =
            getClass Event.isDeleteExpert Event.isEditExpert events expert
    in
    li [ class (divClass ++ " expert") ]
        [ span []
            [ faSet "km.expert" appState
            , text expert.name
            ]
        ]


diffTreeNodeNewExpert : AppState -> List Event -> Event -> Html msg
diffTreeNodeNewExpert appState events event =
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
                    |> diffTreeNewNode "expert" (faSet "km.expert" appState)

        _ ->
            emptyNode


diffTreeNewNode : String -> Html msg -> String -> Html msg
diffTreeNewNode cssClass icon title =
    li [ class <| "ins " ++ cssClass ]
        [ span []
            [ icon
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
