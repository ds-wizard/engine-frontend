module KMEditor.Editor.Models.Events exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import KMEditor.Common.Models.Events
import KMEditor.Editor.Models.Editors exposing (AnswerEditor(..), ExpertEditor(..), QuestionEditor(..), ReferenceEditor(..))
import KMEditor.Editor.Models.Entities exposing (..)
import Random.Pcg exposing (Seed)
import Utils exposing (getUuid)


type Event
    = EditKnowledgeModelEvent EditKnowledgeModelEventData
    | AddChapterEvent AddChapterEventData
    | EditChapterEvent EditChapterEventData
    | DeleteChapterEvent DeleteChapterEventData
    | AddQuestionEvent AddQuestionEventData
    | EditQuestionEvent EditQuestionEventData
    | DeleteQuestionEvent DeleteQuestionEventData
    | AddAnswerEvent AddAnswerEventData
    | EditAnswerEvent EditAnswerEventData
    | DeleteAnswerEvent DeleteAnswerEventData
    | AddReferenceEvent AddReferenceEventData
    | EditReferenceEvent EditReferenceEventData
    | DeleteReferenceEvent DeleteReferenceEventData
    | AddExpertEvent AddExpertEventData
    | EditExpertEvent EditExpertEventData
    | DeleteExpertEvent DeleteExpertEventData
    | AddFollowUpQuestionEvent AddFollowUpQuestionEventData
    | EditFollowUpQuestionEvent EditFollowUpQuestionEventData
    | DeleteFollowUpQuestionEvent DeleteFollowUpQuestionEventData
    | AddAnswerItemTemplateQuestionEvent AddAnswerItemTemplateQuestionEventData
    | EditAnswerItemTemplateQuestionEvent EditAnswerItemTemplateQuestionEventData
    | DeleteAnswerItemTemplateQuestionEvent DeleteAnswerItemTemplateQuestionEventData


type alias EventField a =
    { changed : Bool
    , value : Maybe a
    }


type alias AnswerItemTemplateData =
    { title : String
    , questionIds : List String
    }


type alias CommonEditQuestionEventData =
    { uuid : String
    , questionUuid : String
    , type_ : EventField String
    , title : EventField String
    , shortQuestionUuid : EventField (Maybe String)
    , text : EventField String
    , answerItemTemplate : EventField (Maybe AnswerItemTemplateData)
    , answerIds : EventField (Maybe (List String))
    , expertIds : EventField (List String)
    , referenceIds : EventField (List String)
    }


type alias EditKnowledgeModelEventData =
    { uuid : String
    , kmUuid : String
    , name : EventField String
    , chapterIds : EventField (List String)
    }


type alias AddChapterEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , title : String
    , text : String
    }


type alias EditChapterEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , title : EventField String
    , text : EventField String
    , questionIds : EventField (List String)
    }


type alias DeleteChapterEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    }


type alias AddQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerItemTemplate : Maybe AnswerItemTemplateData
    }


type alias EditQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , type_ : EventField String
    , title : EventField String
    , shortQuestionUuid : EventField (Maybe String)
    , text : EventField String
    , answerItemTemplate : EventField (Maybe AnswerItemTemplateData)
    , answerIds : EventField (Maybe (List String))
    , expertIds : EventField (List String)
    , referenceIds : EventField (List String)
    }


type alias DeleteQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    }


type alias AddAnswerEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , answerUuid : String
    , label : String
    , advice : Maybe String
    }


type alias EditAnswerEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , answerUuid : String
    , label : EventField String
    , advice : EventField (Maybe String)
    , followUpIds : EventField (List String)
    }


type alias DeleteAnswerEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , answerUuid : String
    }


type alias AddReferenceEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , referenceUuid : String
    , chapter : String
    }


type alias EditReferenceEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , referenceUuid : String
    , chapter : EventField String
    }


type alias DeleteReferenceEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , referenceUuid : String
    }


type alias AddExpertEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , expertUuid : String
    , name : String
    , email : String
    }


type alias EditExpertEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , expertUuid : String
    , name : EventField String
    , email : EventField String
    }


type alias DeleteExpertEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , expertUuid : String
    }


type alias AddFollowUpQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , answerUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerItemTemplate : Maybe AnswerItemTemplateData
    }


type alias EditFollowUpQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , answerUuid : String
    , questionUuid : String
    , type_ : EventField String
    , title : EventField String
    , shortQuestionUuid : EventField (Maybe String)
    , text : EventField String
    , answerItemTemplate : EventField (Maybe AnswerItemTemplateData)
    , answerIds : EventField (Maybe (List String))
    , expertIds : EventField (List String)
    , referenceIds : EventField (List String)
    }


type alias DeleteFollowUpQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , answerUuid : String
    , questionUuid : String
    }


type alias AddAnswerItemTemplateQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , parentQuestionUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerItemTemplate : Maybe AnswerItemTemplateData
    }


type alias EditAnswerItemTemplateQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , parentQuestionUuid : String
    , questionUuid : String
    , type_ : EventField String
    , title : EventField String
    , shortQuestionUuid : EventField (Maybe String)
    , text : EventField String
    , answerItemTemplate : EventField (Maybe AnswerItemTemplateData)
    , answerIds : EventField (Maybe (List String))
    , expertIds : EventField (List String)
    , referenceIds : EventField (List String)
    }


type alias DeleteAnswerItemTemplateQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , parentQuestionUuid : String
    , questionUuid : String
    }



{- Common data -}


createEventField : a -> EventField a
createEventField value =
    { changed = True
    , value = Just value
    }


createAnswerItemTemplateData : List String -> AnswerItemTemplate -> AnswerItemTemplateData
createAnswerItemTemplateData questionIds answerItemTemplate =
    { title = answerItemTemplate.title
    , questionIds = questionIds
    }


createCommonEditQuestionEvent : Seed -> QuestionEditor -> ( Seed, CommonEditQuestionEventData )
createCommonEditQuestionEvent seed (QuestionEditor qe) =
    let
        ( uuid, newSeed ) =
            getUuid seed

        maybeAnswerIds =
            case qe.question.type_ of
                "options" ->
                    Just <| List.map (\(AnswerEditor ae) -> ae.answer.uuid) qe.answers

                _ ->
                    Nothing

        maybeAnswerItemTemplate =
            case qe.question.type_ of
                "list" ->
                    let
                        questionIds =
                            List.map (\(QuestionEditor qe) -> qe.question.uuid) qe.answerItemTemplateQuestions
                    in
                    qe.question.answerItemTemplate |> Maybe.map (createAnswerItemTemplateData questionIds)

                _ ->
                    Nothing

        referenceIds =
            List.map (\(ReferenceEditor re) -> re.reference.uuid) qe.references

        expertIds =
            List.map (\(ExpertEditor ee) -> ee.expert.uuid) qe.experts

        eventData =
            { uuid = uuid
            , questionUuid = qe.question.uuid
            , type_ = createEventField qe.question.type_
            , title = createEventField qe.question.title
            , shortQuestionUuid = createEventField qe.question.shortUuid
            , text = createEventField qe.question.text
            , answerItemTemplate = createEventField maybeAnswerItemTemplate
            , answerIds = createEventField maybeAnswerIds
            , referenceIds = createEventField referenceIds
            , expertIds = createEventField expertIds
            }
    in
    ( newSeed, eventData )



{- Event data -}


createEditKnowledgeModelEvent : Seed -> KnowledgeModel -> List String -> ( Event, Seed )
createEditKnowledgeModelEvent seed knowledgeModel chapterIds =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditKnowledgeModelEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , name = createEventField knowledgeModel.name
                , chapterIds = createEventField chapterIds
                }
    in
    ( event, newSeed )


createAddChapterEvent : KnowledgeModel -> Seed -> Chapter -> ( Event, Seed )
createAddChapterEvent knowledgeModel seed chapter =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddChapterEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , title = chapter.title
                , text = chapter.text
                }
    in
    ( event, newSeed )


createEditChapterEvent : KnowledgeModel -> List String -> Seed -> Chapter -> ( Event, Seed )
createEditChapterEvent knowledgeModel questionIds seed chapter =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditChapterEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , title = createEventField chapter.title
                , text = createEventField chapter.text
                , questionIds = createEventField questionIds
                }
    in
    ( event, newSeed )


createDeleteChapterEvent : KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteChapterEvent knowledgeModel seed chapterUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteChapterEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapterUuid
                }
    in
    ( event, newSeed )


createAddQuestionEvent : Chapter -> KnowledgeModel -> Seed -> Question -> ( Event, Seed )
createAddQuestionEvent chapter knowledgeModel seed question =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , type_ = question.type_
                , title = question.title
                , shortQuestionUuid = question.shortUuid
                , text = question.text
                , answerItemTemplate = Nothing
                }
    in
    ( event, newSeed )


createEditQuestionEvent : Chapter -> KnowledgeModel -> Seed -> QuestionEditor -> ( Event, Seed )
createEditQuestionEvent chapter knowledgeModel seed qe =
    let
        ( newSeed, commonData ) =
            createCommonEditQuestionEvent seed qe

        event =
            EditQuestionEvent
                { kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , uuid = commonData.uuid
                , questionUuid = commonData.questionUuid
                , type_ = commonData.type_
                , title = commonData.title
                , shortQuestionUuid = commonData.shortQuestionUuid
                , text = commonData.text
                , answerItemTemplate = commonData.answerItemTemplate
                , answerIds = commonData.answerIds
                , referenceIds = commonData.referenceIds
                , expertIds = commonData.expertIds
                }
    in
    ( event, newSeed )


createDeleteQuestionEvent : Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteQuestionEvent chapter knowledgeModel seed questionUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = questionUuid
                }
    in
    ( event, newSeed )


createAddAnswerEvent : Question -> Chapter -> KnowledgeModel -> Seed -> Answer -> ( Event, Seed )
createAddAnswerEvent question chapter knowledgeModel seed answer =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddAnswerEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , answerUuid = answer.uuid
                , label = answer.label
                , advice = answer.advice
                }
    in
    ( event, newSeed )


createEditAnswerEvent : Question -> Chapter -> KnowledgeModel -> List String -> Seed -> Answer -> ( Event, Seed )
createEditAnswerEvent question chapter knowledgeModel followUpIds seed answer =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditAnswerEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , answerUuid = answer.uuid
                , label = createEventField answer.label
                , advice = createEventField answer.advice
                , followUpIds = createEventField followUpIds
                }
    in
    ( event, newSeed )


createDeleteAnswerEvent : Question -> Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteAnswerEvent question chapter knowledgeModel seed answerUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteAnswerEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , answerUuid = answerUuid
                }
    in
    ( event, newSeed )


createAddReferenceEvent : Question -> Chapter -> KnowledgeModel -> Seed -> Reference -> ( Event, Seed )
createAddReferenceEvent question chapter knowledgeModel seed reference =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddReferenceEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , referenceUuid = reference.uuid
                , chapter = reference.chapter
                }
    in
    ( event, newSeed )


createEditReferenceEvent : Question -> Chapter -> KnowledgeModel -> List String -> Seed -> Reference -> ( Event, Seed )
createEditReferenceEvent question chapter knowledgeModel followupIds seed reference =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditReferenceEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , referenceUuid = reference.uuid
                , chapter = createEventField reference.chapter
                }
    in
    ( event, newSeed )


createDeleteReferenceEvent : Question -> Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteReferenceEvent question chapter knowledgeModel seed referenceUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteReferenceEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , referenceUuid = referenceUuid
                }
    in
    ( event, newSeed )


createAddExpertEvent : Question -> Chapter -> KnowledgeModel -> Seed -> Expert -> ( Event, Seed )
createAddExpertEvent question chapter knowledgeModel seed expert =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddExpertEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , expertUuid = expert.uuid
                , name = expert.name
                , email = expert.email
                }
    in
    ( event, newSeed )


createEditExpertEvent : Question -> Chapter -> KnowledgeModel -> List String -> Seed -> Expert -> ( Event, Seed )
createEditExpertEvent question chapter knowledgeModel followupIds seed expert =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditExpertEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , expertUuid = expert.uuid
                , name = createEventField expert.name
                , email = createEventField expert.email
                }
    in
    ( event, newSeed )


createDeleteExpertEvent : Question -> Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteExpertEvent question chapter knowledgeModel seed expertUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteExpertEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , expertUuid = expertUuid
                }
    in
    ( event, newSeed )


createAddFollowUpQuestionEvent : Answer -> Chapter -> KnowledgeModel -> Seed -> Question -> ( Event, Seed )
createAddFollowUpQuestionEvent answer chapter knowledgeModel seed question =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddFollowUpQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , answerUuid = answer.uuid
                , questionUuid = question.uuid
                , type_ = question.type_
                , title = question.title
                , shortQuestionUuid = question.shortUuid
                , text = question.text
                , answerItemTemplate = Nothing
                }
    in
    ( event, newSeed )


createEditFollowUpQuestionEvent : Answer -> Chapter -> KnowledgeModel -> Seed -> QuestionEditor -> ( Event, Seed )
createEditFollowUpQuestionEvent answer chapter knowledgeModel seed qe =
    let
        ( newSeed, commonData ) =
            createCommonEditQuestionEvent seed qe

        event =
            EditFollowUpQuestionEvent
                { kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , answerUuid = answer.uuid
                , uuid = commonData.uuid
                , questionUuid = commonData.questionUuid
                , type_ = commonData.type_
                , title = commonData.title
                , shortQuestionUuid = commonData.shortQuestionUuid
                , text = commonData.text
                , answerItemTemplate = commonData.answerItemTemplate
                , answerIds = commonData.answerIds
                , referenceIds = commonData.referenceIds
                , expertIds = commonData.expertIds
                }
    in
    ( event, newSeed )


createDeleteFollowUpQuestionEvent : Answer -> Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteFollowUpQuestionEvent answer chapter knowledgeModel seed questionUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteFollowUpQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , answerUuid = answer.uuid
                , questionUuid = questionUuid
                }
    in
    ( event, newSeed )


createAddAnswerItemTemplateQuestionEvent : Question -> Chapter -> KnowledgeModel -> Seed -> Question -> ( Event, Seed )
createAddAnswerItemTemplateQuestionEvent parentQuestion chapter knowledgeModel seed question =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            AddAnswerItemTemplateQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , parentQuestionUuid = parentQuestion.uuid
                , questionUuid = question.uuid
                , type_ = question.type_
                , title = question.title
                , shortQuestionUuid = question.shortUuid
                , text = question.text
                , answerItemTemplate = Nothing
                }
    in
    ( event, newSeed )


createEditAnswerItemTemplateQuestionEvent : Question -> Chapter -> KnowledgeModel -> Seed -> QuestionEditor -> ( Event, Seed )
createEditAnswerItemTemplateQuestionEvent parentQuestion chapter knowledgeModel seed qe =
    let
        ( newSeed, commonData ) =
            createCommonEditQuestionEvent seed qe

        event =
            EditAnswerItemTemplateQuestionEvent
                { kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , parentQuestionUuid = parentQuestion.uuid
                , uuid = commonData.uuid
                , questionUuid = commonData.questionUuid
                , type_ = commonData.type_
                , title = commonData.title
                , shortQuestionUuid = commonData.shortQuestionUuid
                , text = commonData.text
                , answerItemTemplate = commonData.answerItemTemplate
                , answerIds = commonData.answerIds
                , referenceIds = commonData.referenceIds
                , expertIds = commonData.expertIds
                }
    in
    ( event, newSeed )


createDeleteAnswerItemTemplateQuestionEvent : Question -> Chapter -> KnowledgeModel -> Seed -> String -> ( Event, Seed )
createDeleteAnswerItemTemplateQuestionEvent parentQuestion chapter knowledgeModel seed questionUuid =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteAnswerItemTemplateQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , parentQuestionUuid = parentQuestion.uuid
                , questionUuid = questionUuid
                }
    in
    ( event, newSeed )


encodeEvents : List Event -> Encode.Value
encodeEvents events =
    Encode.list (List.map encodeEvent events)


encodeEvent : Event -> Encode.Value
encodeEvent event =
    case event of
        EditKnowledgeModelEvent data ->
            encodeEditKnowledgeModelEvent data

        AddChapterEvent data ->
            encodeAddChapterEvent data

        EditChapterEvent data ->
            encodeEditChapterEvent data

        DeleteChapterEvent data ->
            encodeDeleteChapterEvent data

        AddQuestionEvent data ->
            encodeAddQuestionEvent data

        EditQuestionEvent data ->
            encodeEditQuestionEvent data

        DeleteQuestionEvent data ->
            encodeDeleteQuestionEvent data

        AddAnswerEvent data ->
            encodeAddAnswerEvent data

        EditAnswerEvent data ->
            encodeEditAnswerEvent data

        DeleteAnswerEvent data ->
            encodeDeleteAnswerEvent data

        AddReferenceEvent data ->
            encodeAddReferenceEvent data

        EditReferenceEvent data ->
            encodeEditReferenceEvent data

        DeleteReferenceEvent data ->
            encodeDeleteReferenceEvent data

        AddExpertEvent data ->
            encodeAddExpertEvent data

        EditExpertEvent data ->
            encodeEditExpertEvent data

        DeleteExpertEvent data ->
            encodeDeleteExpertEvent data

        AddFollowUpQuestionEvent data ->
            encodeAddFollowUpQuestionEvent data

        EditFollowUpQuestionEvent data ->
            encodeEditFollowUpQuestionEvent data

        DeleteFollowUpQuestionEvent data ->
            encodeDeleteFollowUpQuestionEvent data

        AddAnswerItemTemplateQuestionEvent data ->
            encodeAddAnswerItemTemplateQuestionEvent data

        EditAnswerItemTemplateQuestionEvent data ->
            encodeEditAnswerItemTemplateQuestionEvent data

        DeleteAnswerItemTemplateQuestionEvent data ->
            encodeDeleteAnswerItemTemplateQuestionEvent data


encodeEventField : (a -> Encode.Value) -> EventField a -> Encode.Value
encodeEventField encode field =
    case ( field.changed, field.value ) of
        ( True, Just value ) ->
            Encode.object
                [ ( "changed", Encode.bool True )
                , ( "value", encode value )
                ]

        _ ->
            Encode.object
                [ ( "changed", Encode.bool False )
                ]


encodeEditKnowledgeModelEvent : EditKnowledgeModelEventData -> Encode.Value
encodeEditKnowledgeModelEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditKnowledgeModelEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "name", encodeEventField Encode.string data.name )
        , ( "chapterIds", encodeEventField (Encode.list << List.map Encode.string) data.chapterIds )
        ]


encodeAddChapterEvent : AddChapterEventData -> Encode.Value
encodeAddChapterEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddChapterEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "title", Encode.string data.title )
        , ( "text", Encode.string data.text )
        ]


encodeEditChapterEvent : EditChapterEventData -> Encode.Value
encodeEditChapterEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditChapterEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "title", encodeEventField Encode.string data.title )
        , ( "text", encodeEventField Encode.string data.text )
        , ( "questionIds", encodeEventField (Encode.list << List.map Encode.string) data.questionIds )
        ]


encodeDeleteChapterEvent : DeleteChapterEventData -> Encode.Value
encodeDeleteChapterEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteChapterEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        ]


encodeAddQuestionEvent : AddQuestionEventData -> Encode.Value
encodeAddQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", maybe Encode.string data.shortQuestionUuid )
        , ( "text", Encode.string data.text )
        , ( "answerItemTemplate", maybe encodeAnswerItemTemplateData data.answerItemTemplate )
        ]


encodeEditQuestionEvent : EditQuestionEventData -> Encode.Value
encodeEditQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", encodeEventField Encode.string data.type_ )
        , ( "title", encodeEventField Encode.string data.title )
        , ( "shortQuestionUuid", encodeEventField (maybe Encode.string) data.shortQuestionUuid )
        , ( "text", encodeEventField Encode.string data.text )
        , ( "answerItemTemplate", encodeEventField (maybe encodeAnswerItemTemplateData) data.answerItemTemplate )
        , ( "answerIds", encodeEventField (maybe (Encode.list << List.map Encode.string)) data.answerIds )
        , ( "expertIds", encodeEventField (Encode.list << List.map Encode.string) data.expertIds )
        , ( "referenceIds", encodeEventField (Encode.list << List.map Encode.string) data.referenceIds )
        ]


encodeAnswerItemTemplateData : AnswerItemTemplateData -> Encode.Value
encodeAnswerItemTemplateData data =
    Encode.object
        [ ( "title", Encode.string data.title )
        , ( "questionIds", (Encode.list << List.map Encode.string) data.questionIds )
        ]


encodeDeleteQuestionEvent : DeleteQuestionEventData -> Encode.Value
encodeDeleteQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        ]


encodeAddAnswerEvent : AddAnswerEventData -> Encode.Value
encodeAddAnswerEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddAnswerEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "label", Encode.string data.label )
        , ( "advice", maybe Encode.string data.advice )
        ]


encodeEditAnswerEvent : EditAnswerEventData -> Encode.Value
encodeEditAnswerEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditAnswerEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "label", encodeEventField Encode.string data.label )
        , ( "advice", encodeEventField (maybe Encode.string) data.advice )
        , ( "followUpIds", encodeEventField (Encode.list << List.map Encode.string) data.followUpIds )
        ]


encodeDeleteAnswerEvent : DeleteAnswerEventData -> Encode.Value
encodeDeleteAnswerEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteAnswerEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        ]


encodeAddReferenceEvent : AddReferenceEventData -> Encode.Value
encodeAddReferenceEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddReferenceEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "referenceUuid", Encode.string data.referenceUuid )
        , ( "chapter", Encode.string data.chapter )
        ]


encodeEditReferenceEvent : EditReferenceEventData -> Encode.Value
encodeEditReferenceEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditReferenceEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "referenceUuid", Encode.string data.referenceUuid )
        , ( "chapter", encodeEventField Encode.string data.chapter )
        ]


encodeDeleteReferenceEvent : DeleteReferenceEventData -> Encode.Value
encodeDeleteReferenceEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteReferenceEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "referenceUuid", Encode.string data.referenceUuid )
        ]


encodeAddExpertEvent : AddExpertEventData -> Encode.Value
encodeAddExpertEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddExpertEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "expertUuid", Encode.string data.expertUuid )
        , ( "name", Encode.string data.name )
        , ( "email", Encode.string data.email )
        ]


encodeEditExpertEvent : EditExpertEventData -> Encode.Value
encodeEditExpertEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditExpertEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "expertUuid", Encode.string data.expertUuid )
        , ( "name", encodeEventField Encode.string data.name )
        , ( "email", encodeEventField Encode.string data.email )
        ]


encodeDeleteExpertEvent : DeleteExpertEventData -> Encode.Value
encodeDeleteExpertEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteExpertEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "expertUuid", Encode.string data.expertUuid )
        ]


encodeAddFollowUpQuestionEvent : AddFollowUpQuestionEventData -> Encode.Value
encodeAddFollowUpQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddFollowUpQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", maybe Encode.string data.shortQuestionUuid )
        , ( "text", Encode.string data.text )
        , ( "answerItemTemplate", maybe encodeAnswerItemTemplateData data.answerItemTemplate )
        ]


encodeEditFollowUpQuestionEvent : EditFollowUpQuestionEventData -> Encode.Value
encodeEditFollowUpQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditFollowUpQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", encodeEventField Encode.string data.type_ )
        , ( "title", encodeEventField Encode.string data.title )
        , ( "shortQuestionUuid", encodeEventField (maybe Encode.string) data.shortQuestionUuid )
        , ( "text", encodeEventField Encode.string data.text )
        , ( "answerItemTemplate", encodeEventField (maybe encodeAnswerItemTemplateData) data.answerItemTemplate )
        , ( "answerIds", encodeEventField (maybe (Encode.list << List.map Encode.string)) data.answerIds )
        , ( "expertIds", encodeEventField (Encode.list << List.map Encode.string) data.expertIds )
        , ( "referenceIds", encodeEventField (Encode.list << List.map Encode.string) data.referenceIds )
        ]


encodeDeleteFollowUpQuestionEvent : DeleteFollowUpQuestionEventData -> Encode.Value
encodeDeleteFollowUpQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteFollowUpQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        ]


encodeAddAnswerItemTemplateQuestionEvent : AddAnswerItemTemplateQuestionEventData -> Encode.Value
encodeAddAnswerItemTemplateQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "AddAnswerItemTemplateQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "parentQuestionUuid", Encode.string data.parentQuestionUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", maybe Encode.string data.shortQuestionUuid )
        , ( "text", Encode.string data.text )
        , ( "answerItemTemplate", maybe encodeAnswerItemTemplateData data.answerItemTemplate )
        ]


encodeEditAnswerItemTemplateQuestionEvent : EditAnswerItemTemplateQuestionEventData -> Encode.Value
encodeEditAnswerItemTemplateQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditAnswerItemTemplateQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "parentQuestionUuid", Encode.string data.parentQuestionUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", encodeEventField Encode.string data.type_ )
        , ( "title", encodeEventField Encode.string data.title )
        , ( "shortQuestionUuid", encodeEventField (maybe Encode.string) data.shortQuestionUuid )
        , ( "text", encodeEventField Encode.string data.text )
        , ( "answerItemTemplate", encodeEventField (maybe encodeAnswerItemTemplateData) data.answerItemTemplate )
        , ( "answerIds", encodeEventField (maybe (Encode.list << List.map Encode.string)) data.answerIds )
        , ( "expertIds", encodeEventField (Encode.list << List.map Encode.string) data.expertIds )
        , ( "referenceIds", encodeEventField (Encode.list << List.map Encode.string) data.referenceIds )
        ]


encodeDeleteAnswerItemTemplateQuestionEvent : DeleteAnswerItemTemplateQuestionEventData -> Encode.Value
encodeDeleteAnswerItemTemplateQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteAnswerItemTemplateQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "parentQuestionUuid", Encode.string data.parentQuestionUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        ]


eventDecoder : Decoder Event
eventDecoder =
    Decode.field "eventType" Decode.string
        |> Decode.andThen eventDecoderByType


eventDecoderByType : String -> Decoder Event
eventDecoderByType eventType =
    case eventType of
        "EditKnowledgeModelEvent" ->
            editKnowledgeModelEventDecoder

        "AddChapterEvent" ->
            addChapterEventDecoder

        "EditChapterEvent" ->
            editChapterEventDecoder

        "DeleteChapterEvent" ->
            deleteChapterEventDecoder

        "AddQuestionEvent" ->
            addQuestionEventDecoder

        "EditQuestionEvent" ->
            editQuestionEventDecoder

        "DeleteQuestionEvent" ->
            deleteQuestionEventDecoder

        "AddAnswerEvent" ->
            addAnswerEventDecoder

        "EditAnswerEvent" ->
            editAnswerEventDecoder

        "DeleteAnswerEvent" ->
            deleteAnswerEventDecoder

        "AddReferenceEvent" ->
            addReferenceEventDecoder

        "EditReferenceEvent" ->
            editReferenceEventDecoder

        "DeleteReferenceEvent" ->
            deleteReferenceEventDecoder

        "AddExpertEvent" ->
            addExpertEventDecoder

        "EditExpertEvent" ->
            editExpertEventDecoder

        "DeleteExpertEvent" ->
            deleteExpertEventDecoder

        "AddFollowUpQuestionEvent" ->
            addFollowUpQuestionEventDecoder

        "EditFollowUpQuestionEvent" ->
            editFollowUpQuestionEventDecoder

        "DeleteFollowUpQuestionEvent" ->
            deleteFollowUpQuestionEventDecoder

        "AddAnswerItemTemplateQuestionEvent" ->
            addAnswerItemTemplateQuestionEventDecoder

        "EditAnswerItemTemplateQuestionEvent" ->
            editAnswerItemTemplateQuestionEventDecoder

        "DeleteAnswerItemTemplateQuestionEvent" ->
            deleteAnswerItemTemplateQuestionEventDecoder

        _ ->
            Decode.fail <| "Unknown event type: " ++ eventType


eventFieldDecoder : Decoder a -> Decoder (EventField a)
eventFieldDecoder decoder =
    decode EventField
        |> required "changed" Decode.bool
        |> optional "value" (Decode.nullable decoder) Nothing


answerItemTemplateDecoder : Decoder AnswerItemTemplateData
answerItemTemplateDecoder =
    decode AnswerItemTemplateData
        |> required "title" Decode.string
        |> required "questionIds" (Decode.list Decode.string)


editKnowledgeModelEventDecoder : Decoder Event
editKnowledgeModelEventDecoder =
    decode EditKnowledgeModelEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "chapterIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditKnowledgeModelEvent


addChapterEventDecoder : Decoder Event
addChapterEventDecoder =
    decode AddChapterEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> Decode.map AddChapterEvent


editChapterEventDecoder : Decoder Event
editChapterEventDecoder =
    decode EditChapterEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "questionIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditChapterEvent


deleteChapterEventDecoder : Decoder Event
deleteChapterEventDecoder =
    decode DeleteChapterEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> Decode.map DeleteChapterEvent


addQuestionEventDecoder : Decoder Event
addQuestionEventDecoder =
    decode AddQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "shortQuestionUuid" (Decode.nullable Decode.string)
        |> required "text" Decode.string
        |> required "answerItemTemplate" (Decode.nullable answerItemTemplateDecoder)
        |> Decode.map AddQuestionEvent


editQuestionEventDecoder : Decoder Event
editQuestionEventDecoder =
    decode EditQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" (eventFieldDecoder Decode.string)
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "shortQuestionUuid" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "answerItemTemplate" (eventFieldDecoder (Decode.nullable answerItemTemplateDecoder))
        |> required "answerIds" (eventFieldDecoder (Decode.nullable (Decode.list Decode.string)))
        |> required "expertIds" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditQuestionEvent


deleteQuestionEventDecoder : Decoder Event
deleteQuestionEventDecoder =
    decode DeleteQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> Decode.map DeleteQuestionEvent


addAnswerEventDecoder : Decoder Event
addAnswerEventDecoder =
    decode AddAnswerEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> Decode.map AddAnswerEvent


editAnswerEventDecoder : Decoder Event
editAnswerEventDecoder =
    decode EditAnswerEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "label" (eventFieldDecoder Decode.string)
        |> required "advice" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "followUpIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditAnswerEvent


deleteAnswerEventDecoder : Decoder Event
deleteAnswerEventDecoder =
    decode DeleteAnswerEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> Decode.map DeleteAnswerEvent


addReferenceEventDecoder : Decoder Event
addReferenceEventDecoder =
    decode AddReferenceEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "referenceUuid" Decode.string
        |> required "chapter" Decode.string
        |> Decode.map AddReferenceEvent


editReferenceEventDecoder : Decoder Event
editReferenceEventDecoder =
    decode EditReferenceEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "referenceUuid" Decode.string
        |> required "chapter" (eventFieldDecoder Decode.string)
        |> Decode.map EditReferenceEvent


deleteReferenceEventDecoder : Decoder Event
deleteReferenceEventDecoder =
    decode DeleteReferenceEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "referenceUuid" Decode.string
        |> Decode.map DeleteReferenceEvent


addExpertEventDecoder : Decoder Event
addExpertEventDecoder =
    decode AddExpertEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "expertUuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string
        |> Decode.map AddExpertEvent


editExpertEventDecoder : Decoder Event
editExpertEventDecoder =
    decode EditExpertEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "expertUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "email" (eventFieldDecoder Decode.string)
        |> Decode.map EditExpertEvent


deleteExpertEventDecoder : Decoder Event
deleteExpertEventDecoder =
    decode DeleteExpertEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "expertUuid" Decode.string
        |> Decode.map DeleteExpertEvent


addFollowUpQuestionEventDecoder : Decoder Event
addFollowUpQuestionEventDecoder =
    decode AddFollowUpQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "shortQuestionUuid" (Decode.nullable Decode.string)
        |> required "text" Decode.string
        |> required "answerItemTemplate" (Decode.nullable answerItemTemplateDecoder)
        |> Decode.map AddFollowUpQuestionEvent


editFollowUpQuestionEventDecoder : Decoder Event
editFollowUpQuestionEventDecoder =
    decode EditFollowUpQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" (eventFieldDecoder Decode.string)
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "shortQuestionUuid" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "answerItemTemplate" (eventFieldDecoder (Decode.nullable answerItemTemplateDecoder))
        |> required "answerIds" (eventFieldDecoder (Decode.nullable (Decode.list Decode.string)))
        |> required "expertIds" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditFollowUpQuestionEvent


deleteFollowUpQuestionEventDecoder : Decoder Event
deleteFollowUpQuestionEventDecoder =
    decode DeleteFollowUpQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> Decode.map DeleteFollowUpQuestionEvent


addAnswerItemTemplateQuestionEventDecoder : Decoder Event
addAnswerItemTemplateQuestionEventDecoder =
    decode AddAnswerItemTemplateQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "shortQuestionUuid" (Decode.nullable Decode.string)
        |> required "text" Decode.string
        |> required "answerItemTemplate" (Decode.nullable answerItemTemplateDecoder)
        |> Decode.map AddAnswerItemTemplateQuestionEvent


editAnswerItemTemplateQuestionEventDecoder : Decoder Event
editAnswerItemTemplateQuestionEventDecoder =
    decode EditAnswerItemTemplateQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> required "type" (eventFieldDecoder Decode.string)
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "shortQuestionUuid" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "answerItemTemplate" (eventFieldDecoder (Decode.nullable answerItemTemplateDecoder))
        |> required "answerIds" (eventFieldDecoder (Decode.nullable (Decode.list Decode.string)))
        |> required "expertIds" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceIds" (eventFieldDecoder (Decode.list Decode.string))
        |> Decode.map EditAnswerItemTemplateQuestionEvent


deleteAnswerItemTemplateQuestionEventDecoder : Decoder Event
deleteAnswerItemTemplateQuestionEventDecoder =
    decode DeleteAnswerItemTemplateQuestionEventData
        |> required "uuid" Decode.string
        |> required "kmUuid" Decode.string
        |> required "chapterUuid" Decode.string
        |> required "answerUuid" Decode.string
        |> required "questionUuid" Decode.string
        |> Decode.map DeleteAnswerItemTemplateQuestionEvent


getEventUuid : Event -> String
getEventUuid event =
    case event of
        EditKnowledgeModelEvent data ->
            data.uuid

        AddChapterEvent data ->
            data.uuid

        EditChapterEvent data ->
            data.uuid

        DeleteChapterEvent data ->
            data.uuid

        AddQuestionEvent data ->
            data.uuid

        EditQuestionEvent data ->
            data.uuid

        DeleteQuestionEvent data ->
            data.uuid

        AddAnswerEvent data ->
            data.uuid

        EditAnswerEvent data ->
            data.uuid

        DeleteAnswerEvent data ->
            data.uuid

        AddReferenceEvent data ->
            data.uuid

        EditReferenceEvent data ->
            data.uuid

        DeleteReferenceEvent data ->
            data.uuid

        AddExpertEvent data ->
            data.uuid

        EditExpertEvent data ->
            data.uuid

        DeleteExpertEvent data ->
            data.uuid

        AddFollowUpQuestionEvent data ->
            data.uuid

        EditFollowUpQuestionEvent data ->
            data.uuid

        DeleteFollowUpQuestionEvent data ->
            data.uuid

        AddAnswerItemTemplateQuestionEvent data ->
            data.uuid

        EditAnswerItemTemplateQuestionEvent data ->
            data.uuid

        DeleteAnswerItemTemplateQuestionEvent data ->
            data.uuid


getEventFieldValue : EventField a -> Maybe a
getEventFieldValue eventField =
    if eventField.changed then
        eventField.value
    else
        Nothing


getEventFieldValueWithDefault : EventField a -> a -> a
getEventFieldValueWithDefault eventField default =
    getEventFieldValue eventField |> Maybe.withDefault default


getEventEntityVisibleName : Event -> Maybe String
getEventEntityVisibleName event =
    case event of
        EditKnowledgeModelEvent data ->
            getEventFieldValue data.name

        AddChapterEvent data ->
            Just data.title

        EditChapterEvent data ->
            getEventFieldValue data.title

        AddQuestionEvent data ->
            Just data.title

        EditQuestionEvent data ->
            getEventFieldValue data.title

        AddAnswerEvent data ->
            Just data.label

        EditAnswerEvent data ->
            getEventFieldValue data.label

        AddReferenceEvent data ->
            Just data.chapter

        EditReferenceEvent data ->
            getEventFieldValue data.chapter

        AddExpertEvent data ->
            Just data.name

        EditExpertEvent data ->
            getEventFieldValue data.name

        AddFollowUpQuestionEvent data ->
            Just data.title

        EditFollowUpQuestionEvent data ->
            getEventFieldValue data.title

        _ ->
            Nothing


isEditChapter : Chapter -> Event -> Bool
isEditChapter chapter event =
    case event of
        EditChapterEvent data ->
            data.chapterUuid == chapter.uuid

        _ ->
            False


isDeleteChapter : Chapter -> Event -> Bool
isDeleteChapter chapter event =
    case event of
        DeleteChapterEvent data ->
            data.chapterUuid == chapter.uuid

        _ ->
            False


isEditQuestion : Question -> Event -> Bool
isEditQuestion question event =
    case event of
        EditQuestionEvent data ->
            data.questionUuid == question.uuid

        EditFollowUpQuestionEvent data ->
            data.questionUuid == question.uuid

        _ ->
            False


isDeleteQuestion : Question -> Event -> Bool
isDeleteQuestion question event =
    case event of
        DeleteQuestionEvent data ->
            data.questionUuid == question.uuid

        DeleteFollowUpQuestionEvent data ->
            data.questionUuid == question.uuid

        _ ->
            False


isEditAnswer : Answer -> Event -> Bool
isEditAnswer answer event =
    case event of
        EditAnswerEvent data ->
            data.answerUuid == answer.uuid

        _ ->
            False


isDeleteAnswer : Answer -> Event -> Bool
isDeleteAnswer answer event =
    case event of
        DeleteAnswerEvent data ->
            data.answerUuid == answer.uuid

        _ ->
            False


isEditReference : Reference -> Event -> Bool
isEditReference reference event =
    case event of
        EditReferenceEvent data ->
            data.referenceUuid == reference.uuid

        _ ->
            False


isDeleteReference : Reference -> Event -> Bool
isDeleteReference reference event =
    case event of
        DeleteReferenceEvent data ->
            data.referenceUuid == reference.uuid

        _ ->
            False


isEditExpert : Expert -> Event -> Bool
isEditExpert expert event =
    case event of
        EditExpertEvent data ->
            data.expertUuid == expert.uuid

        _ ->
            False


isDeleteExpert : Expert -> Event -> Bool
isDeleteExpert expert event =
    case event of
        DeleteExpertEvent data ->
            data.expertUuid == expert.uuid

        _ ->
            False


isAddChapter : KnowledgeModel -> Event -> Bool
isAddChapter km event =
    case event of
        AddChapterEvent data ->
            data.kmUuid == km.uuid

        _ ->
            False


isAddQuestion : Chapter -> Event -> Bool
isAddQuestion chapter event =
    case event of
        AddQuestionEvent data ->
            data.chapterUuid == chapter.uuid

        _ ->
            False


isAddAnswer : Question -> Event -> Bool
isAddAnswer question event =
    case event of
        AddAnswerEvent data ->
            data.questionUuid == question.uuid

        _ ->
            False


isAddExpert : Question -> Event -> Bool
isAddExpert question event =
    case event of
        AddExpertEvent data ->
            data.questionUuid == question.uuid

        _ ->
            False


isAddReference : Question -> Event -> Bool
isAddReference question event =
    case event of
        AddReferenceEvent data ->
            data.questionUuid == question.uuid

        _ ->
            False


isAddFollowUpQuestion : Answer -> Event -> Bool
isAddFollowUpQuestion answer event =
    case event of
        AddFollowUpQuestionEvent data ->
            data.answerUuid == answer.uuid

        _ ->
            False
