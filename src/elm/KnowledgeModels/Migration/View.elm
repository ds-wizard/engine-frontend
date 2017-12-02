module KnowledgeModels.Migration.View exposing (..)

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (formResultView)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Migration.Models exposing (Model)
import KnowledgeModels.Migration.Msgs exposing (Msg(..))
import KnowledgeModels.Models.Migration exposing (Migration, MigrationStateType(..))
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div [ detailContainerClassWith "knowledge-models-migration" ]
        [ pageHeader "Migration" []
        , formResultView model.conflict
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.migration of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success migration ->
            migrationView model migration


migrationView : Model -> Migration -> Html Msgs.Msg
migrationView model migration =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "Migration state is corrupted." ]

        runningStateMessage =
            div [ class "alert alert-warning" ]
                [ text "Migration is still running, try again later." ]

        view =
            case migration.migrationState.stateType of
                ConflictState ->
                    migration.migrationState.targetEvent
                        |> Maybe.map (getEventView model migration)
                        |> Maybe.withDefault errorMessage

                CompletedState ->
                    viewCompletedMigration model

                RunningState ->
                    runningStateMessage

                _ ->
                    errorMessage
    in
    view


getEventView : Model -> Migration -> Event -> Html Msgs.Msg
getEventView model migration event =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "The event is not connected to any entity in the knowledge model." ]
    in
    case event of
        EditKnowledgeModelEvent data ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff data
                |> viewEvent model "Edit knowledge model"

        AddChapterEvent data ->
            viewAddChapterDiff data
                |> viewEvent model "Add chapter"

        EditChapterEvent data ->
            getChapter migration.currentKnowledgeModel data.chapterUuid
                |> Maybe.map (viewEditChapterDiff data)
                |> Maybe.map (viewEvent model "Edit chapter")
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent data ->
            getChapter migration.currentKnowledgeModel data.chapterUuid
                |> Maybe.map viewDeleteChapterDiff
                |> Maybe.map (viewEvent model "Delete chapter")
                |> Maybe.withDefault errorMessage

        AddQuestionEvent data ->
            viewAddQuestionDiff data
                |> viewEvent model "Add question"

        EditQuestionEvent data ->
            getQuestion migration.currentKnowledgeModel data.questionUuid
                |> Maybe.map (viewEditQuestionDiff data)
                |> Maybe.map (viewEvent model "Edit question")
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent data ->
            getQuestion migration.currentKnowledgeModel data.questionUuid
                |> Maybe.map viewDeleteQuestionDiff
                |> Maybe.map (viewEvent model "Delete question")
                |> Maybe.withDefault errorMessage

        AddAnswerEvent data ->
            viewAddAnswerDiff data
                |> viewEvent model "Add answer"

        EditAnswerEvent data ->
            getAnswer migration.currentKnowledgeModel data.answerUuid
                |> Maybe.map (viewEditAnswerDiff data)
                |> Maybe.map (viewEvent model "Edit answer")
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent data ->
            getAnswer migration.currentKnowledgeModel data.answerUuid
                |> Maybe.map viewDeleteAnswerDiff
                |> Maybe.map (viewEvent model "Delete answer")
                |> Maybe.withDefault errorMessage

        AddReferenceEvent data ->
            viewAddReferenceDiff data
                |> viewEvent model "Add reference"

        EditReferenceEvent data ->
            getReference migration.currentKnowledgeModel data.referenceUuid
                |> Maybe.map (viewEditReferenceDiff data)
                |> Maybe.map (viewEvent model "Edit reference")
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent data ->
            getReference migration.currentKnowledgeModel data.referenceUuid
                |> Maybe.map viewDeleteReferenceDiff
                |> Maybe.map (viewEvent model "Delete reference")
                |> Maybe.withDefault errorMessage

        AddExpertEvent data ->
            viewAddExpertDiff data
                |> viewEvent model "Add expert"

        EditExpertEvent data ->
            getExpert migration.currentKnowledgeModel data.expertUuid
                |> Maybe.map (viewEditExpertDiff data)
                |> Maybe.map (viewEvent model "Edit expert")
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent data ->
            getExpert migration.currentKnowledgeModel data.expertUuid
                |> Maybe.map viewDeleteExpertDiff
                |> Maybe.map (viewEvent model "Delete expert")
                |> Maybe.withDefault errorMessage

        AddFollowUpQuestionEvent data ->
            viewAddQuestionDiff data
                |> viewEvent model "Add follow-up question"

        EditFollowUpQuestionEvent data ->
            getQuestion migration.currentKnowledgeModel data.questionUuid
                |> Maybe.map (viewEditQuestionDiff data)
                |> Maybe.map (viewEvent model "Edit follow-up question")
                |> Maybe.withDefault errorMessage

        DeleteFollowUpQuestionEvent data ->
            getQuestion migration.currentKnowledgeModel data.questionUuid
                |> Maybe.map viewDeleteQuestionDiff
                |> Maybe.map (viewEvent model "Delete follow-up question")
                |> Maybe.withDefault errorMessage


viewEvent : Model -> String -> Html Msgs.Msg -> Html Msgs.Msg
viewEvent model name diffView =
    div []
        [ h3 [] [ text name ]
        , div [ class "well" ]
            [ diffView
            , formActions model
            ]
        ]


viewEditKnowledgeModelDiff : EditKnowledgeModelEventData -> KnowledgeModel -> Html Msgs.Msg
viewEditKnowledgeModelDiff event km =
    let
        originalChapters =
            List.map .uuid km.chapters

        chapterNames =
            Dict.fromList <| List.map (\c -> ( c.uuid, c.title )) km.chapters

        fieldDiff =
            viewDiff <| List.map3 (,,) [ "Name" ] [ km.name ] [ event.name ]

        chaptersDiff =
            viewDiffChildren "Chapters" originalChapters event.chapterIds chapterNames
    in
    div []
        (fieldDiff ++ [ chaptersDiff ])


viewAddChapterDiff : AddChapterEventData -> Html Msgs.Msg
viewAddChapterDiff event =
    let
        fields =
            List.map2 (,) [ "Title", "Text" ] [ event.title, event.text ]
    in
    div []
        (viewAdd fields)


viewEditChapterDiff : EditChapterEventData -> Chapter -> Html Msgs.Msg
viewEditChapterDiff event chapter =
    let
        originalQuestions =
            List.map .uuid chapter.questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( q.uuid, q.title )) chapter.questions

        fieldDiff =
            viewDiff <| List.map3 (,,) [ "Title", "Text" ] [ chapter.title, chapter.text ] [ event.title, event.text ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions event.questionIds questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteChapterDiff : Chapter -> Html Msgs.Msg
viewDeleteChapterDiff chapter =
    let
        originalQuestions =
            List.map .uuid chapter.questions

        questionNames =
            List.map (\q -> q.title) chapter.questions

        fieldDiff =
            viewDelete <| List.map2 (,) [ "Title", "Text" ] [ chapter.title, chapter.text ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddQuestionDiff : { a | title : String, shortQuestionUuid : Maybe String, text : String } -> Html Msgs.Msg
viewAddQuestionDiff event =
    let
        fields =
            List.map2 (,)
                [ "Title", "Short UUID", "Text" ]
                [ event.title, event.shortQuestionUuid |> Maybe.withDefault "", event.text ]
    in
    div []
        (viewAdd fields)


viewEditQuestionDiff : { a | title : String, shortQuestionUuid : Maybe String, text : String, answerIds : List String, referenceIds : List String, expertIds : List String } -> Question -> Html Msgs.Msg
viewEditQuestionDiff event question =
    let
        originalAnswers =
            List.map .uuid question.answers

        answerNames =
            Dict.fromList <| List.map (\a -> ( a.uuid, a.label )) question.answers

        originalReferences =
            List.map .uuid question.references

        referenceNames =
            Dict.fromList <| List.map (\r -> ( r.uuid, r.chapter )) question.references

        originalExperts =
            List.map .uuid question.experts

        expertNames =
            Dict.fromList <| List.map (\e -> ( e.uuid, e.name )) question.experts

        fields =
            List.map3 (,,)
                [ "Title", "Short UUID", "Text" ]
                [ question.title, question.shortUuid |> Maybe.withDefault "", question.text ]
                [ event.title, event.shortQuestionUuid |> Maybe.withDefault "", event.text ]

        fieldDiff =
            viewDiff fields

        answersDiff =
            viewDiffChildren "Questions" originalAnswers event.answerIds answerNames

        referencesDiff =
            viewDiffChildren "References" originalReferences event.referenceIds referenceNames

        expertsDiff =
            viewDiffChildren "Experts" originalExperts event.expertIds expertNames
    in
    div []
        (fieldDiff ++ [ answersDiff, referencesDiff, expertsDiff ])


viewDeleteQuestionDiff : Question -> Html Msgs.Msg
viewDeleteQuestionDiff question =
    let
        fields =
            List.map2 (,)
                [ "Title", "Short UUID", "Text" ]
                [ question.title, question.shortUuid |> Maybe.withDefault "", question.text ]

        fieldDiff =
            viewDelete fields

        answersDiff =
            viewDeletedChildren "Answers" <| List.map .label question.answers

        referencesDiff =
            viewDeletedChildren "References" <| List.map .chapter question.references

        expertsDiff =
            viewDeletedChildren "Experts" <| List.map .name question.experts
    in
    div []
        (fieldDiff ++ [ answersDiff, referencesDiff, expertsDiff ])


viewAddAnswerDiff : AddAnswerEventData -> Html Msgs.Msg
viewAddAnswerDiff event =
    let
        fields =
            List.map2 (,) [ "Label", "Advice" ] [ event.label, event.advice |> Maybe.withDefault "" ]
    in
    div []
        (viewAdd fields)


viewEditAnswerDiff : EditAnswerEventData -> Answer -> Html Msgs.Msg
viewEditAnswerDiff event answer =
    let
        originalQuestions =
            List.map .uuid <| getFollowUpQuestions answer

        questionNames =
            Dict.fromList <| List.map (\q -> ( q.uuid, q.title )) <| getFollowUpQuestions answer

        fieldDiff =
            viewDiff <| List.map3 (,,) [ "Label", "Advice" ] [ answer.label, answer.advice |> Maybe.withDefault "" ] [ event.label, event.advice |> Maybe.withDefault "" ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions event.followUpIds questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteAnswerDiff : Answer -> Html Msgs.Msg
viewDeleteAnswerDiff answer =
    let
        originalQuestions =
            List.map .uuid <| getFollowUpQuestions answer

        questionNames =
            List.map (\q -> q.title) <| getFollowUpQuestions answer

        fieldDiff =
            viewDelete <| List.map2 (,) [ "Label", "Advice" ] [ answer.label, answer.advice |> Maybe.withDefault "" ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddReferenceDiff : AddReferenceEventData -> Html Msgs.Msg
viewAddReferenceDiff event =
    div []
        (viewAdd <| List.map2 (,) [ "Chapter" ] [ event.chapter ])


viewEditReferenceDiff : EditReferenceEventData -> Reference -> Html Msgs.Msg
viewEditReferenceDiff event reference =
    div []
        (viewDiff <| List.map3 (,,) [ "Chapter" ] [ reference.chapter ] [ event.chapter ])


viewDeleteReferenceDiff : Reference -> Html Msgs.Msg
viewDeleteReferenceDiff reference =
    div []
        (viewDelete <| List.map2 (,) [ "Chapter" ] [ reference.chapter ])


viewAddExpertDiff : AddExpertEventData -> Html Msgs.Msg
viewAddExpertDiff event =
    div []
        (viewAdd <| List.map2 (,) [ "Name", "Email" ] [ event.name, event.email ])


viewEditExpertDiff : EditExpertEventData -> Expert -> Html Msgs.Msg
viewEditExpertDiff event expert =
    div []
        (viewDiff <| List.map3 (,,) [ "Name", "Email" ] [ expert.name, expert.email ] [ event.name, event.email ])


viewDeleteExpertDiff : Expert -> Html Msgs.Msg
viewDeleteExpertDiff expert =
    div []
        (viewDelete <| List.map2 (,) [ "Name", "Email" ] [ expert.name, expert.email ])


viewDiff : List ( String, String, String ) -> List (Html Msgs.Msg)
viewDiff changes =
    List.map
        (\( fieldName, originalValue, newValue ) ->
            let
                content =
                    if originalValue == newValue then
                        [ div [ class "form-value" ] [ text originalValue ] ]
                    else
                        [ div [ class "form-value" ]
                            [ div [] [ del [] [ text originalValue ] ]
                            , div [] [ ins [] [ text newValue ] ]
                            ]
                        ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewAdd : List ( String, String ) -> List (Html Msgs.Msg)
viewAdd changes =
    List.map
        (\( fieldName, newValue ) ->
            let
                content =
                    [ div [ class "form-value" ]
                        [ div [] [ ins [] [ text newValue ] ]
                        ]
                    ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewDelete : List ( String, String ) -> List (Html Msgs.Msg)
viewDelete changes =
    List.map
        (\( fieldName, newValue ) ->
            let
                content =
                    [ div [ class "form-value" ]
                        [ div [] [ del [] [ text newValue ] ]
                        ]
                    ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewDiffChildren : String -> List String -> List String -> Dict String String -> Html Msgs.Msg
viewDiffChildren fieldName originalOrder newOrder childrenNames =
    let
        viewChildren ulClass uuids =
            ul [ class ulClass ]
                (List.map
                    (\uuid ->
                        Dict.get uuid childrenNames
                            |> Maybe.map (text >> List.singleton >> li [])
                            |> Maybe.withDefault emptyNode
                    )
                    uuids
                )

        diff =
            if List.length originalOrder == 0 then
                div [ class "form-value" ] [ text "-" ]
            else if originalOrder == newOrder then
                div [ class "form-value" ]
                    [ viewChildren "" originalOrder
                    ]
            else
                div [ class "form-value" ]
                    [ viewChildren "del" originalOrder
                    , viewChildren "ins" newOrder
                    ]
    in
    childrenView fieldName diff


viewDeletedChildren : String -> List String -> Html Msgs.Msg
viewDeletedChildren fieldName children =
    childrenView fieldName <|
        ul [ class "del" ]
            (List.map (\child -> li [] [ text child ]) children)


childrenView : String -> Html Msgs.Msg -> Html Msgs.Msg
childrenView fieldName diffView =
    div [ class "form-group" ]
        [ label [ class "control-label" ]
            [ text fieldName ]
        , div [ class "form-value" ]
            [ diffView ]
        ]


formActions : Model -> Html Msgs.Msg
formActions model =
    let
        actionsDisabled =
            case model.conflict of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning", onClick RejectEvent, disabled actionsDisabled ]
            [ text "Reject" ]
        , button [ class "btn btn-success", onClick ApplyEvent, disabled actionsDisabled ]
            [ text "Apply" ]
        ]
        |> Html.map Msgs.KnowledgeModelsMigrationMsg


viewCompletedMigration : Model -> Html Msgs.Msg
viewCompletedMigration model =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check-square-o" ] [] ]
        , p []
            [ text "Migration successfully completed."
            , br [] []
            , text "You can publish the new version now."
            ]
        , div [ class "text-right" ]
            [ linkTo (KnowledgeModelsPublish model.branchUuid)
                [ class "btn btn-primary" ]
                [ text "Publish"
                , i [ class "fa fa-long-arrow-right", style [ ( "margin-left", "10px" ) ] ] []
                ]
            ]
        ]
