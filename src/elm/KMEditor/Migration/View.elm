module KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View.FormResult as FormResult
import Common.View.Page as Page
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
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "col KMEditor__Migration" ]
        [ div [] [ Page.header "Migration" [] ]
        , FormResult.view model.conflict
        , Page.actionResultView (migrationView wrapMsg model) model.migration
        ]


migrationView : (Msg -> Msgs.Msg) -> Model -> Migration -> Html Msgs.Msg
migrationView wrapMsg model migration =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "Migration state is corrupted." ]

        runningStateMessage =
            div [ class "alert alert-warning" ]
                [ text "Migration is still running, try again later." ]

        currentView =
            case migration.migrationState.stateType of
                ConflictState ->
                    let
                        conflictView =
                            migration.migrationState.targetEvent
                                |> Maybe.map (getEventView wrapMsg model migration)
                                |> Maybe.map (List.singleton >> div [ class "col-8" ])
                                |> Maybe.withDefault (div [ class "col-12" ] [ errorMessage ])

                        diffTree =
                            migration.migrationState.targetEvent
                                |> Maybe.map (List.singleton >> diffTreeView migration.currentKnowledgeModel)
                                |> Maybe.map (List.singleton >> div [ class "col-4" ])
                                |> Maybe.withDefault emptyNode
                    in
                    div [ class "row" ]
                        [ migrationSummary migration, conflictView, diffTree ]

                CompletedState ->
                    viewCompletedMigration model

                RunningState ->
                    runningStateMessage

                _ ->
                    errorMessage
    in
    currentView


migrationSummary : Migration -> Html Msgs.Msg
migrationSummary migration =
    div [ class "col-12" ]
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
        AddKnowledgeModelEvent _ _ ->
            -- AddKnowledgeModelEvent should never appear in migrations
            emptyNode

        EditKnowledgeModelEvent eventData _ ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff eventData
                |> viewEvent wrapMsg model "Edit knowledge model"

        AddTagEvent eventData _ ->
            viewAddTagDiff eventData
                |> viewEvent wrapMsg model "Add tag"

        EditTagEvent eventData _ ->
            getTag migration.currentKnowledgeModel eventData.tagUuid
                |> Maybe.map (viewEditTagDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit tag")
                |> Maybe.withDefault errorMessage

        DeleteTagEvent eventData _ ->
            getTag migration.currentKnowledgeModel eventData.tagUuid
                |> Maybe.map viewDeleteTagDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete tag")
                |> Maybe.withDefault errorMessage

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
            viewAddQuestionDiff migration.currentKnowledgeModel eventData
                |> viewEvent wrapMsg model "Add question"

        EditQuestionEvent eventData _ ->
            getQuestion migration.currentKnowledgeModel (getEditQuestionUuid eventData)
                |> Maybe.map (viewEditQuestionDiff migration.currentKnowledgeModel eventData)
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
        , div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                [ diffView
                , formActions wrapMsg model
                ]
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
            viewDiff <| List.map3 (\a b c -> ( a, b, c )) [ "Name" ] [ km.name ] [ getEventFieldValueWithDefault event.name km.name ]

        chaptersDiff =
            viewDiffChildren "Chapters" originalChapters (getEventFieldValueWithDefault event.chapterUuids originalChapters) chapterNames

        originalTags =
            List.map .uuid km.tags

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) km.tags

        tagsDiff =
            viewDiffChildren "Tags" originalTags (getEventFieldValueWithDefault event.tagUuids originalTags) tagNames
    in
    div []
        (fieldDiff ++ [ chaptersDiff, tagsDiff ])


viewAddTagDiff : AddTagEventData -> Html Msgs.Msg
viewAddTagDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Name", "Description", "Color" ] [ event.name, event.description |> Maybe.withDefault "", event.color ]
    in
    div []
        (viewAdd fields)


viewEditTagDiff : EditTagEventData -> Tag -> Html Msgs.Msg
viewEditTagDiff event tag =
    let
        fieldDiff =
            List.map3 (\a b c -> ( a, b, c ))
                [ "Name", "Description", "Color" ]
                [ tag.name, Maybe.withDefault "" tag.description, tag.color ]
                [ getEventFieldValueWithDefault event.name tag.name
                , getEventFieldValueWithDefault event.description tag.description |> Maybe.withDefault ""
                , getEventFieldValueWithDefault event.color tag.color
                ]
    in
    div []
        (viewDiff fieldDiff)


viewDeleteTagDiff : Tag -> Html Msgs.Msg
viewDeleteTagDiff tag =
    let
        fieldDiff =
            List.map2 (\a b -> ( a, b )) [ "Name", "Description", "Color" ] [ tag.name, tag.description |> Maybe.withDefault "", tag.color ]
    in
    div []
        (viewDelete fieldDiff)


viewAddChapterDiff : AddChapterEventData -> Html Msgs.Msg
viewAddChapterDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Title", "Text" ] [ event.title, event.text ]
    in
    div []
        (viewAdd fields)


viewEditChapterDiff : EditChapterEventData -> Chapter -> Html Msgs.Msg
viewEditChapterDiff event chapter =
    let
        originalQuestions =
            List.map getQuestionUuid chapter.questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( getQuestionUuid q, getQuestionTitle q )) chapter.questions

        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ "Title", "Text" ]
                    [ chapter.title, chapter.text ]
                    [ getEventFieldValueWithDefault event.title chapter.title
                    , getEventFieldValueWithDefault event.text chapter.text
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (getEventFieldValueWithDefault event.questionUuids originalQuestions) questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteChapterDiff : Chapter -> Html Msgs.Msg
viewDeleteChapterDiff chapter =
    let
        questionNames =
            List.map getQuestionTitle chapter.questions

        fieldDiff =
            viewDelete <| List.map2 (\a b -> ( a, b )) [ "Title", "Text" ] [ chapter.title, chapter.text ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddQuestionDiff : KnowledgeModel -> AddQuestionEventData -> Html Msgs.Msg
viewAddQuestionDiff km event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Title", "Text", "Tags" ]
                [ getAddQuestionEventQuestionTypeString event
                , mapAddQuestionEventData .title .title .title event
                , mapAddQuestionEventData .text .text .text event |> Maybe.withDefault ""
                ]

        valueField =
            case event of
                AddValueQuestionEvent data ->
                    [ ( "Value Type", valueQuestionTypeString data.valueType ) ]

                _ ->
                    []

        tagUuids =
            mapAddQuestionEventData .tagUuids .tagUuids .tagUuids event

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) <| km.tags

        originalTags =
            List.map (\_ -> "") tagUuids

        tagsDiff =
            viewDiffChildren "Tags" originalTags tagUuids tagNames
    in
    div []
        (viewAdd (fields ++ valueField) ++ [ tagsDiff ])


viewEditQuestionDiff : KnowledgeModel -> EditQuestionEventData -> Question -> Html Msgs.Msg
viewEditQuestionDiff km event question =
    let
        title =
            mapEditQuestionEventData .title .title .title event

        questionText =
            mapEditQuestionEventData .text .text .text event

        originalAnswers =
            List.map .uuid <| getQuestionAnswers question

        answerNames =
            Dict.fromList <| List.map (\a -> ( a.uuid, a.label )) <| getQuestionAnswers question

        originalReferences =
            List.map getReferenceUuid <| getQuestionReferences question

        referenceUuids =
            mapEditQuestionEventData .referenceUuids .referenceUuids .referenceUuids event

        referenceNames =
            Dict.fromList <| List.map (\r -> ( getReferenceUuid r, getReferenceVisibleName r )) <| getQuestionReferences question

        originalExperts =
            List.map .uuid <| getQuestionExperts question

        expertUuids =
            mapEditQuestionEventData .expertUuids .expertUuids .expertUuids event

        expertNames =
            Dict.fromList <| List.map (\e -> ( e.uuid, e.name )) <| getQuestionExperts question

        fields =
            List.map3 (\a b c -> ( a, b, c ))
                [ "Type", "Title", "Text" ]
                [ getQuestionTypeString question
                , getQuestionTitle question
                , getQuestionText question |> Maybe.withDefault ""
                ]
                [ getEditQuestionEventQuestionTypeString event
                , getEventFieldValueWithDefault title <| getQuestionTitle question
                , getEventFieldValueWithDefault questionText (getQuestionText question) |> Maybe.withDefault ""
                ]

        originalValueType =
            getQuestionValueType question

        valueType =
            mapEditQuestionEventData (\_ -> Nothing) (\_ -> Nothing) (\data -> getEventFieldValue data.valueType) event

        valueDiff =
            case ( originalValueType, valueType ) of
                ( Nothing, Nothing ) ->
                    []

                ( original, new ) ->
                    let
                        originalStr =
                            Maybe.withDefault "" <| Maybe.map valueQuestionTypeString original

                        newStr =
                            Maybe.withDefault originalStr <| Maybe.map valueQuestionTypeString new
                    in
                    [ ( "Value Type", originalStr, newStr ) ]

        fieldDiff =
            viewDiff (fields ++ valueDiff)

        originalTags =
            getQuestionTagUuids question

        tagUuids =
            getEventFieldValueWithDefault (mapEditQuestionEventData .tagUuids .tagUuids .tagUuids event) originalTags

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) <| km.tags

        tagsDiff =
            viewDiffChildren "Tags" originalTags tagUuids tagNames

        answersDiff =
            case event of
                EditOptionsQuestionEvent eventData ->
                    let
                        answerUuids =
                            getEventFieldValueWithDefault eventData.answerUuids originalAnswers
                    in
                    viewDiffChildren "Answers" originalAnswers answerUuids answerNames

                _ ->
                    emptyNode

        referencesDiff =
            viewDiffChildren "References" originalReferences (getEventFieldValueWithDefault referenceUuids originalReferences) referenceNames

        expertsDiff =
            viewDiffChildren "Experts" originalExperts (getEventFieldValueWithDefault expertUuids originalExperts) expertNames
    in
    div []
        (fieldDiff ++ [ tagsDiff, answersDiff, referencesDiff, expertsDiff ])


viewDeleteQuestionDiff : Question -> Html Msgs.Msg
viewDeleteQuestionDiff question =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Title", "Text" ]
                [ getQuestionTitle question, getQuestionText question |> Maybe.withDefault "" ]

        valueField =
            case question of
                ValueQuestion data ->
                    [ ( "Value Type", valueQuestionTypeString data.valueType ) ]

                _ ->
                    []

        fieldDiff =
            viewDelete (fields ++ valueField)

        answersDiff =
            viewDeletedChildren "Answers" <| List.map .label <| getQuestionAnswers question

        referencesDiff =
            viewDeletedChildren "References" <| List.map getReferenceVisibleName <| getQuestionReferences question

        expertsDiff =
            viewDeletedChildren "Experts" <| List.map .name <| getQuestionExperts question
    in
    div []
        (fieldDiff ++ [ answersDiff, referencesDiff, expertsDiff ])


viewAddAnswerDiff : AddAnswerEventData -> Html Msgs.Msg
viewAddAnswerDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Label", "Advice" ] [ event.label, event.advice |> Maybe.withDefault "" ]
    in
    div []
        (viewAdd fields)


viewEditAnswerDiff : EditAnswerEventData -> Answer -> Html Msgs.Msg
viewEditAnswerDiff event answer =
    let
        originalQuestions =
            List.map getQuestionUuid <| getFollowUpQuestions answer

        questionNames =
            Dict.fromList <| List.map (\q -> ( getQuestionUuid q, getQuestionTitle q )) <| getFollowUpQuestions answer

        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ "Label", "Advice" ]
                    [ answer.label, answer.advice |> Maybe.withDefault "" ]
                    [ getEventFieldValueWithDefault event.label answer.label
                    , getEventFieldValueWithDefault event.advice answer.advice |> Maybe.withDefault ""
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (getEventFieldValueWithDefault event.followUpUuids originalQuestions) questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteAnswerDiff : Answer -> Html Msgs.Msg
viewDeleteAnswerDiff answer =
    let
        questionNames =
            List.map getQuestionTitle <| getFollowUpQuestions answer

        fieldDiff =
            viewDelete <| List.map2 (\a b -> ( a, b )) [ "Label", "Advice" ] [ answer.label, answer.advice |> Maybe.withDefault "" ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddReferenceDiff : AddReferenceEventData -> Html Msgs.Msg
viewAddReferenceDiff =
    mapAddReferenceEventData
        viewAddResourcePageReferenceDiff
        viewAddURLReferenceDiff
        viewAddCrossReferenceDiff


viewAddResourcePageReferenceDiff : AddResourcePageReferenceEventData -> Html Msgs.Msg
viewAddResourcePageReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "Short UUID" ] [ "Resource Page", data.shortUuid ])


viewAddURLReferenceDiff : AddURLReferenceEventData -> Html Msgs.Msg
viewAddURLReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "URL", "Label" ] [ "URL", data.url, data.label ])


viewAddCrossReferenceDiff : AddCrossReferenceEventData -> Html Msgs.Msg
viewAddCrossReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "Target UUID", "Description" ] [ "Cross Reference", data.targetUuid, data.description ])


viewEditReferenceDiff : EditReferenceEventData -> Reference -> Html Msgs.Msg
viewEditReferenceDiff event reference =
    case ( event, reference ) of
        ( EditResourcePageReferenceEvent eventData, ResourcePageReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "Short UUID" ]
                        [ "Resource Page", referenceData.shortUuid ]
                        [ "Resource Page", getEventFieldValueWithDefault eventData.shortUuid referenceData.shortUuid ]
                )

        ( EditURLReferenceEvent eventData, URLReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "URL", "Label" ]
                        [ "URL", referenceData.url, referenceData.label ]
                        [ "URL"
                        , getEventFieldValueWithDefault eventData.url referenceData.url
                        , getEventFieldValueWithDefault eventData.label referenceData.label
                        ]
                )

        ( EditCrossReferenceEvent eventData, CrossReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "Target UUID", "Description" ]
                        [ "Cross Reference", referenceData.targetUuid, referenceData.description ]
                        [ "Cross Reference"
                        , getEventFieldValueWithDefault eventData.targetUuid referenceData.targetUuid
                        , getEventFieldValueWithDefault eventData.description referenceData.description
                        ]
                )

        ( otherEvent, otherReference ) ->
            let
                deleteReference =
                    viewDeleteReferenceDiff otherReference

                addReference =
                    mapEditReferenceEventData
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
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Short UUID" ]
                [ "Resource Page", getEventFieldValueWithDefault data.shortUuid "" ]
        )


viewEditURLReferenceDiff : EditURLReferenceEventData -> Html Msgs.Msg
viewEditURLReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ "Type", "URL", "Label" ]
                [ "URL"
                , getEventFieldValueWithDefault data.url ""
                , getEventFieldValueWithDefault data.label ""
                ]
        )


viewEditCrossReferenceDiff : EditCrossReferenceEventData -> Html Msgs.Msg
viewEditCrossReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Target UUID", "Description" ]
                [ "Cross Reference"
                , getEventFieldValueWithDefault data.targetUuid ""
                , getEventFieldValueWithDefault data.description ""
                ]
        )


viewDeleteReferenceDiff : Reference -> Html Msgs.Msg
viewDeleteReferenceDiff =
    mapReferenceData
        viewDeleteResourcePageReferenceDiff
        viewDeleteURLReferenceDiff
        viewDeleteCrossReferenceDiff


viewDeleteResourcePageReferenceDiff : ResourcePageReferenceData -> Html Msgs.Msg
viewDeleteResourcePageReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "Short UUID" ] [ "Resource Page", data.shortUuid ])


viewDeleteURLReferenceDiff : URLReferenceData -> Html Msgs.Msg
viewDeleteURLReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "URL", "Label" ] [ "URL", data.url, data.label ])


viewDeleteCrossReferenceDiff : CrossReferenceData -> Html Msgs.Msg
viewDeleteCrossReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "Target UUID", "Description" ] [ "Cross Reference", data.targetUuid, data.description ])


viewAddExpertDiff : AddExpertEventData -> Html Msgs.Msg
viewAddExpertDiff event =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Name", "Email" ] [ event.name, event.email ])


viewEditExpertDiff : EditExpertEventData -> Expert -> Html Msgs.Msg
viewEditExpertDiff event expert =
    div []
        (viewDiff <|
            List.map3 (\a b c -> ( a, b, c ))
                [ "Name", "Email" ]
                [ expert.name, expert.email ]
                [ getEventFieldValueWithDefault event.name expert.name
                , getEventFieldValueWithDefault event.email expert.email
                ]
        )


viewDeleteExpertDiff : Expert -> Html Msgs.Msg
viewDeleteExpertDiff expert =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Name", "Email" ] [ expert.name, expert.email ])


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
                [ linkTo (KMEditor <| PublishRoute model.branchUuid)
                    [ class "btn btn-primary" ]
                    [ text "Publish"
                    , i [ class "fa fa-long-arrow-right", style "margin-left" "10px" ] []
                    ]
                ]
            ]
        ]
