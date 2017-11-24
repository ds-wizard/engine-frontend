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
    , text : String
    }


type alias EditQuestionEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
    , questionUuid : String
    , type_ : String
    , title : String
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
                , text = question.text
                }
    in
    ( event, newSeed )


createEditQuestionEvent : Chapter -> KnowledgeModel -> Seed -> Question -> ( Event, Seed )
createEditQuestionEvent chapter knowledgeModel seed question =
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
                , text = question.text
                , answerIds = []
                , expertIds = []
                , referenceIds = []
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
    Encode.object
        [ ( "eventType", Encode.string "AddQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "shortuid", Encode.null )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "text", Encode.string data.text )
        ]


encodeEditQuestionEvent : EditQuestionEventData -> Encode.Value
encodeEditQuestionEvent data =
    Encode.object
        [ ( "eventType", Encode.string "EditQuestionEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        , ( "questionUuid", Encode.string data.questionUuid )
        , ( "shortuid", Encode.null )
        , ( "type", Encode.string data.type_ )
        , ( "title", Encode.string data.title )
        , ( "text", Encode.string data.text )
        , ( "answerIds", Encode.null )
        , ( "expertIds", Encode.null )
        , ( "referenceIds", Encode.null )
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
