module KnowledgeModels.Editor.Models.Events exposing (..)

import Json.Encode as Encode exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
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


type alias EditKnowledgeModelEventData =
    { uuid : String
    , kmUuid : String
    , name : String
    , chapterIds : List String
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
    , title : String
    , text : String
    , questionIds : List String
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
    }


type alias EditQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerIds : List String
    , expertIds : List String
    , referenceIds : List String
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
    , label : String
    , advice : Maybe String
    , followUpIds : List String
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
    , chapter : String
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
    , name : String
    , email : String
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
    }


type alias EditFollowUpQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , answerUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerIds : List String
    , expertIds : List String
    , referenceIds : List String
    }


type alias DeleteFollowUpQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , answerUuid : String
    , questionUuid : String
    }


createEditKnowledgeModelEvent : Seed -> KnowledgeModel -> List String -> ( Event, Seed )
createEditKnowledgeModelEvent seed knowledgeModel chapterIds =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditKnowledgeModelEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , name = knowledgeModel.name
                , chapterIds = chapterIds
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
                , title = chapter.title
                , text = chapter.text
                , questionIds = questionIds
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
                }
    in
    ( event, newSeed )


createEditQuestionEvent : Chapter -> KnowledgeModel -> List String -> List String -> List String -> Seed -> Question -> ( Event, Seed )
createEditQuestionEvent chapter knowledgeModel answerIds referenceIds expertIds seed question =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , questionUuid = question.uuid
                , type_ = question.type_
                , title = question.title
                , shortQuestionUuid = question.shortUuid
                , text = question.text
                , answerIds = answerIds
                , referenceIds = referenceIds
                , expertIds = expertIds
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
                , label = answer.label
                , advice = answer.advice
                , followUpIds = followUpIds
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
                , chapter = reference.chapter
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
                , name = expert.name
                , email = expert.email
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
                }
    in
    ( event, newSeed )


createEditFollowUpQuestionEvent : Answer -> Chapter -> KnowledgeModel -> List String -> List String -> List String -> Seed -> Question -> ( Event, Seed )
createEditFollowUpQuestionEvent answer chapter knowledgeModel answerIds referenceIds expertIds seed question =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            EditFollowUpQuestionEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                , answerUuid = answer.uuid
                , questionUuid = question.uuid
                , type_ = question.type_
                , title = question.title
                , shortQuestionUuid = question.shortUuid
                , text = question.text
                , answerIds = answerIds
                , referenceIds = referenceIds
                , expertIds = expertIds
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


encodeEditKnowledgeModelEvent : EditKnowledgeModelEventData -> Encode.Value
encodeEditKnowledgeModelEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditKnowledgeModelEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "name", Encode.string data.name )
        , ( "chapterIds", Encode.list <| List.map Encode.string data.chapterIds )
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
        , ( "title", Encode.string data.title )
        , ( "text", Encode.string data.text )
        , ( "questionIds", Encode.list <| List.map Encode.string data.questionIds )
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
    let
        shortQuestionUuid =
            data.shortQuestionUuid
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "AddQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", shortQuestionUuid )
        , ( "text", Encode.string data.text )
        ]


encodeEditQuestionEvent : EditQuestionEventData -> Encode.Value
encodeEditQuestionEvent data =
    let
        shortQuestionUuid =
            data.shortQuestionUuid
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "EditQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", shortQuestionUuid )
        , ( "text", Encode.string data.text )
        , ( "answerIds", Encode.list <| List.map Encode.string data.answerIds )
        , ( "expertIds", Encode.list <| List.map Encode.string data.expertIds )
        , ( "referenceIds", Encode.list <| List.map Encode.string data.referenceIds )
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
    let
        advice =
            data.advice
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "AddAnswerEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "label", Encode.string data.label )
        , ( "advice", advice )
        ]


encodeEditAnswerEvent : EditAnswerEventData -> Encode.Value
encodeEditAnswerEvent data =
    let
        advice =
            data.advice
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "EditAnswerEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "label", Encode.string data.label )
        , ( "advice", advice )
        , ( "followUpIds", Encode.list <| List.map Encode.string data.followUpIds )
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
        , ( "chapter", Encode.string data.chapter )
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
        , ( "name", Encode.string data.name )
        , ( "email", Encode.string data.email )
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
    let
        shortQuestionUuid =
            data.shortQuestionUuid
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "AddFollowUpQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", shortQuestionUuid )
        , ( "text", Encode.string data.text )
        ]


encodeEditFollowUpQuestionEvent : EditFollowUpQuestionEventData -> Encode.Value
encodeEditFollowUpQuestionEvent data =
    let
        shortQuestionUuid =
            data.shortQuestionUuid
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "eventType", Encode.string "EditFollowUpQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "answerUuid", Encode.string data.answerUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "shortQuestionUuid", shortQuestionUuid )
        , ( "text", Encode.string data.text )
        , ( "answerIds", Encode.list <| List.map Encode.string data.answerIds )
        , ( "expertIds", Encode.list <| List.map Encode.string data.expertIds )
        , ( "referenceIds", Encode.list <| List.map Encode.string data.referenceIds )
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
