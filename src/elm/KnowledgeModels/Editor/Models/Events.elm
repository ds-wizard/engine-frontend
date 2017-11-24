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
    , questionIds : Maybe (List String)
    }


type alias DeleteChapterEventData =
    { uuid : String
    , kmUuid : String
    , chapterUuid : String
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


createDeleteChapterEvent : KnowledgeModel -> Seed -> Chapter -> ( Event, Seed )
createDeleteChapterEvent knowledgeModel seed chapter =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            DeleteChapterEvent
                { uuid = uuid
                , kmUuid = knowledgeModel.uuid
                , chapterUuid = chapter.uuid
                }
    in
    ( event, newSeed )


createEditChapterEvent : KnowledgeModel -> Seed -> Chapter -> ( Event, Seed )
createEditChapterEvent knowledgeModel seed chapter =
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
                , questionIds = Nothing
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
        , ( "questionIds", Encode.null )
        ]


encodeDeleteChapterEvent : DeleteChapterEventData -> Encode.Value
encodeDeleteChapterEvent data =
    Encode.object
        [ ( "eventType", Encode.string "DeleteChapterEvent" )
        , ( "uuid", Encode.string data.uuid )
        , ( "kmUuid", Encode.string data.kmUuid )
        , ( "chapterUuid", Encode.string data.chapterUuid )
        ]
