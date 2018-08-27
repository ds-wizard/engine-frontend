module KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, pageHeader)
import Common.View.Forms exposing (formResultView)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Migration exposing (Migration, MigrationStateType(..))
import KMEditor.Common.View exposing (diffTreeView)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(Publish))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "row KMEditor__Migration" ]
        [ div [ class "col-xs-12" ] [ pageHeader "Migration" [] ]
        , formResultView model.conflict
        , fullPageActionResultView (migrationView wrapMsg model) model.migration
        ]


migrationView : (Msg -> Msgs.Msg) -> Model -> Migration -> Html Msgs.Msg
migrationView wrapMsg model migration =
    let
        errorMessage =
            div [ class "col-xs-12" ]
                [ div [ class "alert alert-danger" ]
                    [ text "Migration state is corrupted." ]
                ]

        runningStateMessage =
            div [ class "col-xs-12" ]
                [ div [ class "alert alert-warning" ]
                    [ text "Migration is still running, try again later." ]
                ]

        view =
            case migration.migrationState.stateType of
                ConflictState ->
                    let
                        conflictView =
                            migration.migrationState.targetEvent
                                |> Maybe.map (getEventView wrapMsg model migration)
                                |> Maybe.map (List.singleton >> div [ class "col-xs-8" ])
                                |> Maybe.withDefault errorMessage

                        diffTree =
                            migration.migrationState.targetEvent
                                |> Maybe.map (List.singleton >> diffTreeView migration.currentKnowledgeModel)
                                |> Maybe.map (List.singleton >> div [ class "col-xs-4" ])
                                |> Maybe.withDefault emptyNode
                    in
                    div []
                        [ migrationSummary migration, conflictView, diffTree ]

                CompletedState ->
                    viewCompletedMigration model

                RunningState ->
                    runningStateMessage

                _ ->
                    errorMessage
    in
    view


migrationSummary : Migration -> Html Msgs.Msg
migrationSummary migration =
    div [ class "col-xs-12" ]
        [ p []
            [ text "Migration of "
            , strong [] [ text migration.currentKnowledgeModel.name ]
            , text " from "
            , code [] [ text migration.branchParentId ]
            , text " to "
            , code [] [ text migration.targetPackageId ]
            , text "."
            ]
        ]


getEventView : (Msg -> Msgs.Msg) -> Model -> Migration -> Event -> Html Msgs.Msg
getEventView wrapMsg model migration event =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "The event is not connected to any entity in the knowledge model." ]
    in
    case event of
        EditKnowledgeModelEvent eventData _ ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff eventData
                |> viewEvent wrapMsg model "Edit knowledge model"

        AddChapterEvent eventData _ ->
            viewAddChapterDiff eventData
                |> viewEvent wrapMsg model "Add chapter"

        EditChapterEvent eventData _ ->
            getChapter migration.currentKnowledgeModel eventData.chapterUuid
                |> Maybe.map (viewEditChapterDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit chapter")
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent eventData _ ->
            getChapter migration.currentKnowledgeModel eventData.chapterUuid
                |> Maybe.map viewDeleteChapterDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete chapter")
                |> Maybe.withDefault errorMessage

        AddQuestionEvent eventData _ ->
            viewAddQuestionDiff eventData
                |> viewEvent wrapMsg model "Add question"

        EditQuestionEvent eventData _ ->
            getQuestion migration.currentKnowledgeModel eventData.questionUuid
                |> Maybe.map (viewEditQuestionDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit question")
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent eventData _ ->
            getQuestion migration.currentKnowledgeModel eventData.questionUuid
                |> Maybe.map viewDeleteQuestionDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete question")
                |> Maybe.withDefault errorMessage

        AddAnswerEvent eventData _ ->
            viewAddAnswerDiff eventData
                |> viewEvent wrapMsg model "Add answer"

        EditAnswerEvent eventData _ ->
            getAnswer migration.currentKnowledgeModel eventData.answerUuid
                |> Maybe.map (viewEditAnswerDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit answer")
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent eventData _ ->
            getAnswer migration.currentKnowledgeModel eventData.answerUuid
                |> Maybe.map viewDeleteAnswerDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete answer")
                |> Maybe.withDefault errorMessage

        AddReferenceEvent eventData _ ->
            viewAddReferenceDiff eventData
                |> viewEvent wrapMsg model "Add reference"

        EditReferenceEvent eventData _ ->
            getReference migration.currentKnowledgeModel (getEditReferenceUuid eventData)
                |> Maybe.map (viewEditReferenceDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit reference")
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent eventData _ ->
            getReference migration.currentKnowledgeModel eventData.referenceUuid
                |> Maybe.map viewDeleteReferenceDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete reference")
                |> Maybe.withDefault errorMessage

        AddExpertEvent eventData _ ->
            viewAddExpertDiff eventData
                |> viewEvent wrapMsg model "Add expert"

        EditExpertEvent eventData _ ->
            getExpert migration.currentKnowledgeModel eventData.expertUuid
                |> Maybe.map (viewEditExpertDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit expert")
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent eventData _ ->
            getExpert migration.currentKnowledgeModel eventData.expertUuid
                |> Maybe.map viewDeleteExpertDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete expert")
                |> Maybe.withDefault errorMessage


viewEvent : (Msg -> Msgs.Msg) -> Model -> String -> Html Msgs.Msg -> Html Msgs.Msg
viewEvent wrapMsg model name diffView =
    div []
        [ h3 [] [ text name ]
        , div [ class "well" ]
            [ diffView
            , formActions wrapMsg model
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
            viewDiff <| List.map3 (,,) [ "Name" ] [ km.name ] [ getEventFieldValueWithDefault event.name km.name ]

        chaptersDiff =
            viewDiffChildren "Chapters" originalChapters (getEventFieldValueWithDefault event.chapterIds originalChapters) chapterNames
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
            viewDiff <|
                List.map3 (,,)
                    [ "Title", "Text" ]
                    [ chapter.title, chapter.text ]
                    [ getEventFieldValueWithDefault event.title chapter.title
                    , getEventFieldValueWithDefault event.text chapter.text
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (getEventFieldValueWithDefault event.questionIds originalQuestions) questionNames
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


viewAddQuestionDiff : { a | title : String, text : Maybe String } -> Html Msgs.Msg
viewAddQuestionDiff event =
    let
        fields =
            List.map2 (,)
                [ "Title", "Text" ]
                [ event.title, event.text |> Maybe.withDefault "" ]
    in
    div []
        (viewAdd fields)


viewEditQuestionDiff : { a | title : EventField String, text : EventField (Maybe String), answerIds : EventField (Maybe (List String)), referenceIds : EventField (List String), expertIds : EventField (List String) } -> Question -> Html Msgs.Msg
viewEditQuestionDiff event question =
    let
        originalAnswers =
            List.map .uuid (question.answers |> Maybe.withDefault [])

        answerNames =
            Dict.fromList <| List.map (\a -> ( a.uuid, a.label )) (question.answers |> Maybe.withDefault [])

        originalReferences =
            List.map getReferenceUuid question.references

        referenceNames =
            Dict.fromList <| List.map (\r -> ( getReferenceUuid r, getReferenceVisibleName r )) question.references

        originalExperts =
            List.map .uuid question.experts

        expertNames =
            Dict.fromList <| List.map (\e -> ( e.uuid, e.name )) question.experts

        fields =
            List.map3 (,,)
                [ "Title", "Text" ]
                [ question.title, question.text |> Maybe.withDefault "" ]
                [ getEventFieldValueWithDefault event.title question.title
                , getEventFieldValueWithDefault event.text question.text |> Maybe.withDefault ""
                ]

        fieldDiff =
            viewDiff fields

        answersDiff =
            case getEventFieldValueWithDefault event.answerIds (Just originalAnswers) of
                Just answerIds ->
                    viewDiffChildren "Answers" originalAnswers answerIds answerNames

                _ ->
                    emptyNode

        referencesDiff =
            viewDiffChildren "References" originalReferences (getEventFieldValueWithDefault event.referenceIds originalReferences) referenceNames

        expertsDiff =
            viewDiffChildren "Experts" originalExperts (getEventFieldValueWithDefault event.expertIds originalExperts) expertNames
    in
    div []
        (fieldDiff ++ [ answersDiff, referencesDiff, expertsDiff ])


viewDeleteQuestionDiff : Question -> Html Msgs.Msg
viewDeleteQuestionDiff question =
    let
        fields =
            List.map2 (,)
                [ "Title", "Text" ]
                [ question.title, question.text |> Maybe.withDefault "" ]

        fieldDiff =
            viewDelete fields

        answersDiff =
            viewDeletedChildren "Answers" <| List.map .label (question.answers |> Maybe.withDefault [])

        referencesDiff =
            viewDeletedChildren "References" <| List.map getReferenceVisibleName question.references

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
            viewDiff <|
                List.map3 (,,)
                    [ "Label", "Advice" ]
                    [ answer.label, answer.advice |> Maybe.withDefault "" ]
                    [ getEventFieldValueWithDefault event.label answer.label
                    , getEventFieldValueWithDefault event.advice answer.advice |> Maybe.withDefault ""
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (getEventFieldValueWithDefault event.followUpIds originalQuestions) questionNames
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
viewAddReferenceDiff =
    addReferenceEventDataByType
        viewAddResourcePageReferenceDiff
        viewAddURLReferenceDiff
        viewAddCrossReferenceDiff


viewAddResourcePageReferenceDiff : AddResourcePageReferenceEventData -> Html Msgs.Msg
viewAddResourcePageReferenceDiff data =
    div []
        (viewAdd <| List.map2 (,) [ "Short UUID" ] [ data.shortUuid ])


viewAddURLReferenceDiff : AddURLReferenceEventData -> Html Msgs.Msg
viewAddURLReferenceDiff data =
    div []
        (viewAdd <| List.map2 (,) [ "URL", "Label" ] [ data.url, data.label ])


viewAddCrossReferenceDiff : AddCrossReferenceEventData -> Html Msgs.Msg
viewAddCrossReferenceDiff data =
    div []
        (viewAdd <| List.map2 (,) [ "Target UUID", "Description" ] [ data.targetUuid, data.description ])


viewEditReferenceDiff : EditReferenceEventData -> Reference -> Html Msgs.Msg
viewEditReferenceDiff event reference =
    case ( event, reference ) of
        ( EditResourcePageReferenceEvent eventData, ResourcePageReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (,,)
                        [ "Short UUID" ]
                        [ referenceData.shortUuid ]
                        [ getEventFieldValueWithDefault eventData.shortUuid referenceData.shortUuid ]
                )

        ( EditURLReferenceEvent eventData, URLReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (,,)
                        [ "URL", "Label" ]
                        [ referenceData.url, referenceData.label ]
                        [ getEventFieldValueWithDefault eventData.url referenceData.url
                        , getEventFieldValueWithDefault eventData.label referenceData.label
                        ]
                )

        ( EditCrossReferenceEvent eventData, CrossReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (,,)
                        [ "Target UUID", "Description" ]
                        [ referenceData.targetUuid, referenceData.description ]
                        [ getEventFieldValueWithDefault eventData.targetUuid referenceData.targetUuid
                        , getEventFieldValueWithDefault eventData.description referenceData.description
                        ]
                )

        ( otherEvent, otherReference ) ->
            let
                deleteReference =
                    viewDeleteReferenceDiff otherReference

                addReference =
                    editReferenceEventDataByType
                        viewEditResourcePageReferenceDiff
                        viewEditURLReferenceDiff
                        viewEditCrossReferenceDiff
                        otherEvent
            in
            div [] [ deleteReference, addReference ]


viewEditResourcePageReferenceDiff : EditResourcePageReferenceEventData -> Html Msgs.Msg
viewEditResourcePageReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (,)
                [ "Short UUID" ]
                [ getEventFieldValueWithDefault data.shortUuid "" ]
        )


viewEditURLReferenceDiff : EditURLReferenceEventData -> Html Msgs.Msg
viewEditURLReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (,)
                [ "URL", "Label" ]
                [ getEventFieldValueWithDefault data.url ""
                , getEventFieldValueWithDefault data.label ""
                ]
        )


viewEditCrossReferenceDiff : EditCrossReferenceEventData -> Html Msgs.Msg
viewEditCrossReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (,)
                [ "Target UUID", "Description" ]
                [ getEventFieldValueWithDefault data.targetUuid ""
                , getEventFieldValueWithDefault data.description ""
                ]
        )


viewDeleteReferenceDiff : Reference -> Html Msgs.Msg
viewDeleteReferenceDiff =
    referenceByType
        viewDeleteResourcePageReferenceDiff
        viewDeleteURLReferenceDiff
        viewDeleteCrossReferenceDiff


viewDeleteResourcePageReferenceDiff : ResourcePageReferenceData -> Html Msgs.Msg
viewDeleteResourcePageReferenceDiff data =
    div []
        (viewDelete <| List.map2 (,) [ "Short UUID" ] [ data.shortUuid ])


viewDeleteURLReferenceDiff : URLReferenceData -> Html Msgs.Msg
viewDeleteURLReferenceDiff data =
    div []
        (viewDelete <| List.map2 (,) [ "URL", "Label" ] [ data.url, data.label ])


viewDeleteCrossReferenceDiff : CrossReferenceData -> Html Msgs.Msg
viewDeleteCrossReferenceDiff data =
    div []
        (viewDelete <| List.map2 (,) [ "Target UUID", "Description" ] [ data.targetUuid, data.description ])


viewAddExpertDiff : AddExpertEventData -> Html Msgs.Msg
viewAddExpertDiff event =
    div []
        (viewAdd <| List.map2 (,) [ "Name", "Email" ] [ event.name, event.email ])


viewEditExpertDiff : EditExpertEventData -> Expert -> Html Msgs.Msg
viewEditExpertDiff event expert =
    div []
        (viewDiff <|
            List.map3 (,,)
                [ "Name", "Email" ]
                [ expert.name, expert.email ]
                [ getEventFieldValueWithDefault event.name expert.name
                , getEventFieldValueWithDefault event.email expert.email
                ]
        )


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


formActions : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
formActions wrapMsg model =
    let
        actionsDisabled =
            case model.conflict of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning", onClick (wrapMsg RejectEvent), disabled actionsDisabled ]
            [ text "Reject" ]
        , button [ class "btn btn-success", onClick (wrapMsg ApplyEvent), disabled actionsDisabled ]
            [ text "Apply" ]
        ]


viewCompletedMigration : Model -> Html Msgs.Msg
viewCompletedMigration model =
    div [ class "col-xs-12" ]
        [ div [ class "jumbotron full-page-error" ]
            [ h1 [ class "display-3" ] [ i [ class "fa fa-check-square-o" ] [] ]
            , p []
                [ text "Migration successfully completed."
                , br [] []
                , text "You can publish the new version now."
                ]
            , div [ class "text-right" ]
                [ linkTo (KMEditor <| Publish model.branchUuid)
                    [ class "btn btn-primary" ]
                    [ text "Publish"
                    , i [ class "fa fa-long-arrow-right", style [ ( "margin-left", "10px" ) ] ] []
                    ]
                ]
            ]
        ]
