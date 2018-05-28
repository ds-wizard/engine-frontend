module KMEditor.Common.Models.Events exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import List.Extra as List
import Random.Pcg exposing (Seed)
import Utils exposing (getUuid)


type Event
    = EditKnowledgeModelEvent EditKnowledgeModelEventData CommonEventData
    | AddChapterEvent AddChapterEventData CommonEventData
    | EditChapterEvent EditChapterEventData CommonEventData
    | DeleteChapterEvent DeleteChapterEventData CommonEventData
    | AddQuestionEvent AddQuestionEventData CommonEventData
    | EditQuestionEvent EditQuestionEventData CommonEventData
    | DeleteQuestionEvent DeleteQuestionEventData CommonEventData
    | AddAnswerEvent AddAnswerEventData CommonEventData
    | EditAnswerEvent EditAnswerEventData CommonEventData
    | DeleteAnswerEvent DeleteAnswerEventData CommonEventData
    | AddReferenceEvent AddReferenceEventData CommonEventData
    | EditReferenceEvent EditReferenceEventData CommonEventData
    | DeleteReferenceEvent DeleteReferenceEventData CommonEventData
    | AddExpertEvent AddExpertEventData CommonEventData
    | EditExpertEvent EditExpertEventData CommonEventData
    | DeleteExpertEvent DeleteExpertEventData CommonEventData


type alias CommonEventData =
    { uuid : String
    , path : Path
    }


type alias EditKnowledgeModelEventData =
    { kmUuid : String
    , name : EventField String
    , chapterIds : EventField (List String)
    }


type alias AddChapterEventData =
    { chapterUuid : String
    , title : String
    , text : String
    }


type alias EditChapterEventData =
    { chapterUuid : String
    , title : EventField String
    , text : EventField String
    , questionIds : EventField (List String)
    }


type alias DeleteChapterEventData =
    { chapterUuid : String
    }


type alias AddQuestionEventData =
    { questionUuid : String
    , type_ : String
    , title : String
    , shortQuestionUuid : Maybe String
    , text : String
    , answerItemTemplate : Maybe AnswerItemTemplateData
    }


type alias EditQuestionEventData =
    { questionUuid : String
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
    { questionUuid : String
    }


type alias AddAnswerEventData =
    { answerUuid : String
    , label : String
    , advice : Maybe String
    }


type alias EditAnswerEventData =
    { answerUuid : String
    , label : EventField String
    , advice : EventField (Maybe String)
    , followUpIds : EventField (List String)
    }


type alias DeleteAnswerEventData =
    { answerUuid : String
    }


type alias AddReferenceEventData =
    { referenceUuid : String
    , chapter : String
    }


type alias EditReferenceEventData =
    { referenceUuid : String
    , chapter : EventField String
    }


type alias DeleteReferenceEventData =
    { referenceUuid : String
    }


type alias AddExpertEventData =
    { expertUuid : String
    , name : String
    , email : String
    }


type alias EditExpertEventData =
    { expertUuid : String
    , name : EventField String
    , email : EventField String
    }


type alias DeleteExpertEventData =
    { expertUuid : String
    }


type PathNode
    = KMPathNode String
    | ChapterPathNode String
    | QuestionPathNode String
    | AnswerPathNode String


type alias Path =
    List PathNode


type alias EventField a =
    { changed : Bool
    , value : Maybe a
    }


type alias AnswerItemTemplateData =
    { title : String
    , questionIds : List String
    }



{- Creating events -}


createEvent : (CommonEventData -> Event) -> Path -> Seed -> ( Event, Seed )
createEvent create path seed =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            create
                { uuid = uuid
                , path = path
                }
    in
    ( event, newSeed )


createEditKnowledgeModelEvent : KnowledgeModelEditor -> Seed -> ( Event, Seed )
createEditKnowledgeModelEvent (KnowledgeModelEditor kme) =
    let
        chapterIds =
            List.map (\(ChapterEditor ce) -> ce.chapter.uuid) kme.chapters

        data =
            { kmUuid = kme.knowledgeModel.uuid
            , name = createEventField kme.knowledgeModel.name
            , chapterIds = createEventField chapterIds
            }
    in
    createEvent (EditKnowledgeModelEvent data) []


createAddChapterEvent : Chapter -> Path -> Seed -> ( Event, Seed )
createAddChapterEvent chapter =
    let
        data =
            { chapterUuid = chapter.uuid
            , title = chapter.title
            , text = chapter.text
            }
    in
    createEvent (AddChapterEvent data)


createEditChapterEvent : ChapterEditor -> Path -> Seed -> ( Event, Seed )
createEditChapterEvent (ChapterEditor ce) =
    let
        questionIds =
            List.map (\(QuestionEditor qe) -> qe.question.uuid) ce.questions

        data =
            { chapterUuid = ce.chapter.uuid
            , title = createEventField ce.chapter.title
            , text = createEventField ce.chapter.text
            , questionIds = createEventField questionIds
            }
    in
    createEvent (EditChapterEvent data)


createDeleteChapterEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteChapterEvent chapterUuid =
    let
        data =
            { chapterUuid = chapterUuid
            }
    in
    createEvent (DeleteChapterEvent data)


createAddQuestionEvent : Question -> Path -> Seed -> ( Event, Seed )
createAddQuestionEvent question =
    let
        data =
            { questionUuid = question.uuid
            , type_ = question.type_
            , title = question.title
            , shortQuestionUuid = question.shortUuid
            , text = question.text
            , answerItemTemplate = Nothing
            }
    in
    createEvent (AddQuestionEvent data)


createEditQuestionEvent : QuestionEditor -> Path -> Seed -> ( Event, Seed )
createEditQuestionEvent (QuestionEditor qe) =
    let
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

        data =
            { questionUuid = qe.question.uuid
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
    createEvent (EditQuestionEvent data)


createDeleteQuestionEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteQuestionEvent questionUuid =
    let
        data =
            { questionUuid = questionUuid
            }
    in
    createEvent (DeleteQuestionEvent data)


createAddAnswerEvent : Answer -> Path -> Seed -> ( Event, Seed )
createAddAnswerEvent answer =
    let
        data =
            { answerUuid = answer.uuid
            , label = answer.label
            , advice = answer.advice
            }
    in
    createEvent (AddAnswerEvent data)


createEditAnswerEvent : AnswerEditor -> Path -> Seed -> ( Event, Seed )
createEditAnswerEvent (AnswerEditor ae) =
    let
        followUpIds =
            List.map (\(QuestionEditor qe) -> qe.question.uuid) ae.followUps

        data =
            { answerUuid = ae.answer.uuid
            , label = createEventField ae.answer.label
            , advice = createEventField ae.answer.advice
            , followUpIds = createEventField followUpIds
            }
    in
    createEvent (EditAnswerEvent data)


createDeleteAnswerEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteAnswerEvent answerUuid =
    let
        data =
            { answerUuid = answerUuid
            }
    in
    createEvent (DeleteAnswerEvent data)


createAddReferenceEvent : Reference -> Path -> Seed -> ( Event, Seed )
createAddReferenceEvent reference =
    let
        data =
            { referenceUuid = reference.uuid
            , chapter = reference.chapter
            }
    in
    createEvent (AddReferenceEvent data)


createEditReferenceEvent : Reference -> Path -> Seed -> ( Event, Seed )
createEditReferenceEvent reference =
    let
        data =
            { referenceUuid = reference.uuid
            , chapter = createEventField reference.chapter
            }
    in
    createEvent (EditReferenceEvent data)


createDeleteReferenceEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteReferenceEvent referenceUuid =
    let
        data =
            { referenceUuid = referenceUuid
            }
    in
    createEvent (DeleteReferenceEvent data)


createAddExpertEvent : Expert -> Path -> Seed -> ( Event, Seed )
createAddExpertEvent expert =
    let
        data =
            { expertUuid = expert.uuid
            , name = expert.name
            , email = expert.email
            }
    in
    createEvent (AddExpertEvent data)


createEditExpertEvent : Expert -> Path -> Seed -> ( Event, Seed )
createEditExpertEvent expert =
    let
        data =
            { expertUuid = expert.uuid
            , name = createEventField expert.name
            , email = createEventField expert.email
            }
    in
    createEvent (EditExpertEvent data)


createDeleteExpertEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteExpertEvent expertUuid =
    let
        data =
            { expertUuid = expertUuid
            }
    in
    createEvent (DeleteExpertEvent data)


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



{- Encoders -}


encodeEvents : List Event -> Encode.Value
encodeEvents events =
    Encode.list (List.map encodeEvent events)


encodeEvent : Event -> Encode.Value
encodeEvent event =
    let
        ( commonData, eventData ) =
            case event of
                EditKnowledgeModelEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditKnowledgeModelEvent eventData )

                AddChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddChapterEvent eventData )

                EditChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditChapterEvent eventData )

                DeleteChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteChapterEvent eventData )

                AddQuestionEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddQuestionEvent eventData )

                EditQuestionEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditQuestionEvent eventData )

                DeleteQuestionEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteQuestionEvent eventData )

                AddAnswerEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddAnswerEvent eventData )

                EditAnswerEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditAnswerEvent eventData )

                DeleteAnswerEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteAnswerEvent eventData )

                AddReferenceEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddReferenceEvent eventData )

                EditReferenceEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditReferenceEvent eventData )

                DeleteReferenceEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteReferenceEvent eventData )

                AddExpertEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddExpertEvent eventData )

                EditExpertEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditExpertEvent eventData )

                DeleteExpertEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteExpertEvent eventData )
    in
    Encode.object <| commonData ++ eventData


encodeCommonData : CommonEventData -> List ( String, Encode.Value )
encodeCommonData data =
    [ ( "uuid", Encode.string data.uuid )
    , ( "path", Encode.list (List.map encodePathNode data.path) )
    ]


encodeEditKnowledgeModelEvent : EditKnowledgeModelEventData -> List ( String, Encode.Value )
encodeEditKnowledgeModelEvent data =
    [ ( "eventType", Encode.string "EditKnowledgeModelEvent" )
    , ( "kmUuid", Encode.string data.kmUuid )
    , ( "name", encodeEventField Encode.string data.name )
    , ( "chapterIds", encodeEventField (Encode.list << List.map Encode.string) data.chapterIds )
    ]


encodeAddChapterEvent : AddChapterEventData -> List ( String, Encode.Value )
encodeAddChapterEvent data =
    [ ( "eventType", Encode.string "AddChapterEvent" )
    , ( "chapterUuid", Encode.string data.chapterUuid )
    , ( "title", Encode.string data.title )
    , ( "text", Encode.string data.text )
    ]


encodeEditChapterEvent : EditChapterEventData -> List ( String, Encode.Value )
encodeEditChapterEvent data =
    [ ( "eventType", Encode.string "EditChapterEvent" )
    , ( "chapterUuid", Encode.string data.chapterUuid )
    , ( "title", encodeEventField Encode.string data.title )
    , ( "text", encodeEventField Encode.string data.text )
    , ( "questionIds", encodeEventField (Encode.list << List.map Encode.string) data.questionIds )
    ]


encodeDeleteChapterEvent : DeleteChapterEventData -> List ( String, Encode.Value )
encodeDeleteChapterEvent data =
    [ ( "eventType", Encode.string "DeleteChapterEvent" )
    , ( "chapterUuid", Encode.string data.chapterUuid )
    ]


encodeAddQuestionEvent : AddQuestionEventData -> List ( String, Encode.Value )
encodeAddQuestionEvent data =
    [ ( "eventType", Encode.string "AddQuestionEvent" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "type", Encode.string data.type_ )
    , ( "title", Encode.string data.title )
    , ( "shortQuestionUuid", maybe Encode.string data.shortQuestionUuid )
    , ( "text", Encode.string data.text )
    , ( "answerItemTemplate", maybe encodeAnswerItemTemplateData data.answerItemTemplate )
    ]


encodeEditQuestionEvent : EditQuestionEventData -> List ( String, Encode.Value )
encodeEditQuestionEvent data =
    [ ( "eventType", Encode.string "EditQuestionEvent" )
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


encodeDeleteQuestionEvent : DeleteQuestionEventData -> List ( String, Encode.Value )
encodeDeleteQuestionEvent data =
    [ ( "eventType", Encode.string "DeleteQuestionEvent" )
    , ( "questionUuid", Encode.string data.questionUuid )
    ]


encodeAddAnswerEvent : AddAnswerEventData -> List ( String, Encode.Value )
encodeAddAnswerEvent data =
    [ ( "eventType", Encode.string "AddAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    , ( "label", Encode.string data.label )
    , ( "advice", maybe Encode.string data.advice )
    ]


encodeEditAnswerEvent : EditAnswerEventData -> List ( String, Encode.Value )
encodeEditAnswerEvent data =
    [ ( "eventType", Encode.string "EditAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    , ( "label", encodeEventField Encode.string data.label )
    , ( "advice", encodeEventField (maybe Encode.string) data.advice )
    , ( "followUpIds", encodeEventField (Encode.list << List.map Encode.string) data.followUpIds )
    ]


encodeDeleteAnswerEvent : DeleteAnswerEventData -> List ( String, Encode.Value )
encodeDeleteAnswerEvent data =
    [ ( "eventType", Encode.string "DeleteAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    ]


encodeAddReferenceEvent : AddReferenceEventData -> List ( String, Encode.Value )
encodeAddReferenceEvent data =
    [ ( "eventType", Encode.string "AddReferenceEvent" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "chapter", Encode.string data.chapter )
    ]


encodeEditReferenceEvent : EditReferenceEventData -> List ( String, Encode.Value )
encodeEditReferenceEvent data =
    [ ( "eventType", Encode.string "EditReferenceEvent" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "chapter", encodeEventField Encode.string data.chapter )
    ]


encodeDeleteReferenceEvent : DeleteReferenceEventData -> List ( String, Encode.Value )
encodeDeleteReferenceEvent data =
    [ ( "eventType", Encode.string "DeleteReferenceEvent" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    ]


encodeAddExpertEvent : AddExpertEventData -> List ( String, Encode.Value )
encodeAddExpertEvent data =
    [ ( "eventType", Encode.string "AddExpertEvent" )
    , ( "expertUuid", Encode.string data.expertUuid )
    , ( "name", Encode.string data.name )
    , ( "email", Encode.string data.email )
    ]


encodeEditExpertEvent : EditExpertEventData -> List ( String, Encode.Value )
encodeEditExpertEvent data =
    [ ( "eventType", Encode.string "EditExpertEvent" )
    , ( "expertUuid", Encode.string data.expertUuid )
    , ( "name", encodeEventField Encode.string data.name )
    , ( "email", encodeEventField Encode.string data.email )
    ]


encodeDeleteExpertEvent : DeleteExpertEventData -> List ( String, Encode.Value )
encodeDeleteExpertEvent data =
    [ ( "eventType", Encode.string "DeleteExpertEvent" )
    , ( "expertUuid", Encode.string data.expertUuid )
    ]


encodePathNode : PathNode -> Encode.Value
encodePathNode node =
    case node of
        KMPathNode uuid ->
            createEncodedPathNode "km" uuid

        ChapterPathNode uuid ->
            createEncodedPathNode "chapter" uuid

        QuestionPathNode uuid ->
            createEncodedPathNode "question" uuid

        AnswerPathNode uuid ->
            createEncodedPathNode "answer" uuid


createEncodedPathNode : String -> String -> Encode.Value
createEncodedPathNode pathNodeType uuid =
    Encode.object
        [ ( "type", Encode.string pathNodeType )
        , ( "uuid", Encode.string uuid )
        ]


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


encodeAnswerItemTemplateData : AnswerItemTemplateData -> Encode.Value
encodeAnswerItemTemplateData data =
    Encode.object
        [ ( "title", Encode.string data.title )
        , ( "questionIds", (Encode.list << List.map Encode.string) data.questionIds )
        ]



{- Decoders -}


eventDecoder : Decoder Event
eventDecoder =
    Decode.field "eventType" Decode.string
        |> Decode.andThen eventDecoderByType


eventDecoderByType : String -> Decoder Event
eventDecoderByType eventType =
    case eventType of
        "EditKnowledgeModelEvent" ->
            Decode.map2 EditKnowledgeModelEvent editKnowledgeModelEventDecoder commonEventDataDecoder

        "AddChapterEvent" ->
            Decode.map2 AddChapterEvent addChapterEventDecoder commonEventDataDecoder

        "EditChapterEvent" ->
            Decode.map2 EditChapterEvent editChapterEventDecoder commonEventDataDecoder

        "DeleteChapterEvent" ->
            Decode.map2 DeleteChapterEvent deleteChapterEventDecoder commonEventDataDecoder

        "AddQuestionEvent" ->
            Decode.map2 AddQuestionEvent addQuestionEventDecoder commonEventDataDecoder

        "EditQuestionEvent" ->
            Decode.map2 EditQuestionEvent editQuestionEventDecoder commonEventDataDecoder

        "DeleteQuestionEvent" ->
            Decode.map2 DeleteQuestionEvent deleteQuestionEventDecoder commonEventDataDecoder

        "AddAnswerEvent" ->
            Decode.map2 AddAnswerEvent addAnswerEventDecoder commonEventDataDecoder

        "EditAnswerEvent" ->
            Decode.map2 EditAnswerEvent editAnswerEventDecoder commonEventDataDecoder

        "DeleteAnswerEvent" ->
            Decode.map2 DeleteAnswerEvent deleteAnswerEventDecoder commonEventDataDecoder

        "AddReferenceEvent" ->
            Decode.map2 AddReferenceEvent addReferenceEventDecoder commonEventDataDecoder

        "EditReferenceEvent" ->
            Decode.map2 EditReferenceEvent editReferenceEventDecoder commonEventDataDecoder

        "DeleteReferenceEvent" ->
            Decode.map2 DeleteReferenceEvent deleteReferenceEventDecoder commonEventDataDecoder

        "AddExpertEvent" ->
            Decode.map2 AddExpertEvent addExpertEventDecoder commonEventDataDecoder

        "EditExpertEvent" ->
            Decode.map2 EditExpertEvent editExpertEventDecoder commonEventDataDecoder

        "DeleteExpertEvent" ->
            Decode.map2 DeleteExpertEvent deleteExpertEventDecoder commonEventDataDecoder

        _ ->
            Decode.fail <| "Unknown event type: " ++ eventType


commonEventDataDecoder : Decoder CommonEventData
commonEventDataDecoder =
    decode CommonEventData
        |> required "uuid" Decode.string
        |> required "path" pathDecoder


editKnowledgeModelEventDecoder : Decoder EditKnowledgeModelEventData
editKnowledgeModelEventDecoder =
    decode EditKnowledgeModelEventData
        |> required "kmUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "chapterIds" (eventFieldDecoder (Decode.list Decode.string))


addChapterEventDecoder : Decoder AddChapterEventData
addChapterEventDecoder =
    decode AddChapterEventData
        |> required "chapterUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string


editChapterEventDecoder : Decoder EditChapterEventData
editChapterEventDecoder =
    decode EditChapterEventData
        |> required "chapterUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "questionIds" (eventFieldDecoder (Decode.list Decode.string))


deleteChapterEventDecoder : Decoder DeleteChapterEventData
deleteChapterEventDecoder =
    decode DeleteChapterEventData
        |> required "chapterUuid" Decode.string


addQuestionEventDecoder : Decoder AddQuestionEventData
addQuestionEventDecoder =
    decode AddQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "shortQuestionUuid" (Decode.nullable Decode.string)
        |> required "text" Decode.string
        |> required "answerItemTemplate" (Decode.nullable answerItemTemplateDecoder)


editQuestionEventDecoder : Decoder EditQuestionEventData
editQuestionEventDecoder =
    decode EditQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "type" (eventFieldDecoder Decode.string)
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "shortQuestionUuid" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "answerItemTemplate" (eventFieldDecoder (Decode.nullable answerItemTemplateDecoder))
        |> required "answerIds" (eventFieldDecoder (Decode.nullable (Decode.list Decode.string)))
        |> required "expertIds" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceIds" (eventFieldDecoder (Decode.list Decode.string))


deleteQuestionEventDecoder : Decoder DeleteQuestionEventData
deleteQuestionEventDecoder =
    decode DeleteQuestionEventData
        |> required "questionUuid" Decode.string


addAnswerEventDecoder : Decoder AddAnswerEventData
addAnswerEventDecoder =
    decode AddAnswerEventData
        |> required "answerUuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)


editAnswerEventDecoder : Decoder EditAnswerEventData
editAnswerEventDecoder =
    decode EditAnswerEventData
        |> required "answerUuid" Decode.string
        |> required "label" (eventFieldDecoder Decode.string)
        |> required "advice" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "followUpIds" (eventFieldDecoder (Decode.list Decode.string))


deleteAnswerEventDecoder : Decoder DeleteAnswerEventData
deleteAnswerEventDecoder =
    decode DeleteAnswerEventData
        |> required "answerUuid" Decode.string


addReferenceEventDecoder : Decoder AddReferenceEventData
addReferenceEventDecoder =
    decode AddReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "chapter" Decode.string


editReferenceEventDecoder : Decoder EditReferenceEventData
editReferenceEventDecoder =
    decode EditReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "chapter" (eventFieldDecoder Decode.string)


deleteReferenceEventDecoder : Decoder DeleteReferenceEventData
deleteReferenceEventDecoder =
    decode DeleteReferenceEventData
        |> required "referenceUuid" Decode.string


addExpertEventDecoder : Decoder AddExpertEventData
addExpertEventDecoder =
    decode AddExpertEventData
        |> required "expertUuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string


editExpertEventDecoder : Decoder EditExpertEventData
editExpertEventDecoder =
    decode EditExpertEventData
        |> required "expertUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "email" (eventFieldDecoder Decode.string)


deleteExpertEventDecoder : Decoder DeleteExpertEventData
deleteExpertEventDecoder =
    decode DeleteExpertEventData
        |> required "expertUuid" Decode.string


pathDecoder : Decoder Path
pathDecoder =
    Decode.list pathNodeDecoder


pathNodeDecoder : Decoder PathNode
pathNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen pathNodeDecoderByType


pathNodeDecoderByType : String -> Decoder PathNode
pathNodeDecoderByType pathNodeType =
    case pathNodeType of
        "km" ->
            Decode.map KMPathNode pathNodeUuidDecoder

        "chapter" ->
            Decode.map ChapterPathNode pathNodeUuidDecoder

        "question" ->
            Decode.map QuestionPathNode pathNodeUuidDecoder

        "answer" ->
            Decode.map AnswerPathNode pathNodeUuidDecoder

        _ ->
            Decode.fail <| "Unknown path node type: " ++ pathNodeType


pathNodeUuidDecoder : Decoder String
pathNodeUuidDecoder =
    Decode.field "uuid" Decode.string


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



{- Helpers -}


getEventUuid : Event -> String
getEventUuid event =
    case event of
        EditKnowledgeModelEvent _ commonData ->
            commonData.uuid

        AddChapterEvent _ commonData ->
            commonData.uuid

        EditChapterEvent _ commonData ->
            commonData.uuid

        DeleteChapterEvent _ commonData ->
            commonData.uuid

        AddQuestionEvent _ commonData ->
            commonData.uuid

        EditQuestionEvent _ commonData ->
            commonData.uuid

        DeleteQuestionEvent _ commonData ->
            commonData.uuid

        AddAnswerEvent _ commonData ->
            commonData.uuid

        EditAnswerEvent _ commonData ->
            commonData.uuid

        DeleteAnswerEvent _ commonData ->
            commonData.uuid

        AddReferenceEvent _ commonData ->
            commonData.uuid

        EditReferenceEvent _ commonData ->
            commonData.uuid

        DeleteReferenceEvent _ commonData ->
            commonData.uuid

        AddExpertEvent _ commonData ->
            commonData.uuid

        EditExpertEvent _ commonData ->
            commonData.uuid

        DeleteExpertEvent _ commonData ->
            commonData.uuid


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
        EditKnowledgeModelEvent eventData _ ->
            getEventFieldValue eventData.name

        AddChapterEvent eventData _ ->
            Just eventData.title

        EditChapterEvent eventData _ ->
            getEventFieldValue eventData.title

        AddQuestionEvent eventData _ ->
            Just eventData.title

        EditQuestionEvent eventData _ ->
            getEventFieldValue eventData.title

        AddAnswerEvent eventData _ ->
            Just eventData.label

        EditAnswerEvent eventData _ ->
            getEventFieldValue eventData.label

        AddReferenceEvent eventData _ ->
            Just eventData.chapter

        EditReferenceEvent eventData _ ->
            getEventFieldValue eventData.chapter

        AddExpertEvent eventData _ ->
            Just eventData.name

        EditExpertEvent eventData _ ->
            getEventFieldValue eventData.name

        _ ->
            Nothing


isEditChapter : Chapter -> Event -> Bool
isEditChapter chapter event =
    case event of
        EditChapterEvent eventData _ ->
            eventData.chapterUuid == chapter.uuid

        _ ->
            False


isDeleteChapter : Chapter -> Event -> Bool
isDeleteChapter chapter event =
    case event of
        DeleteChapterEvent eventData _ ->
            eventData.chapterUuid == chapter.uuid

        _ ->
            False


isEditQuestion : Question -> Event -> Bool
isEditQuestion question event =
    case event of
        EditQuestionEvent eventData _ ->
            eventData.questionUuid == question.uuid

        _ ->
            False


isDeleteQuestion : Question -> Event -> Bool
isDeleteQuestion question event =
    case event of
        DeleteQuestionEvent eventData _ ->
            eventData.questionUuid == question.uuid

        _ ->
            False


isEditAnswer : Answer -> Event -> Bool
isEditAnswer answer event =
    case event of
        EditAnswerEvent eventData _ ->
            eventData.answerUuid == answer.uuid

        _ ->
            False


isDeleteAnswer : Answer -> Event -> Bool
isDeleteAnswer answer event =
    case event of
        DeleteAnswerEvent eventData _ ->
            eventData.answerUuid == answer.uuid

        _ ->
            False


isEditReference : Reference -> Event -> Bool
isEditReference reference event =
    case event of
        EditReferenceEvent eventData _ ->
            eventData.referenceUuid == reference.uuid

        _ ->
            False


isDeleteReference : Reference -> Event -> Bool
isDeleteReference reference event =
    case event of
        DeleteReferenceEvent eventData _ ->
            eventData.referenceUuid == reference.uuid

        _ ->
            False


isEditExpert : Expert -> Event -> Bool
isEditExpert expert event =
    case event of
        EditExpertEvent eventData _ ->
            eventData.expertUuid == expert.uuid

        _ ->
            False


isDeleteExpert : Expert -> Event -> Bool
isDeleteExpert expert event =
    case event of
        DeleteExpertEvent eventData _ ->
            eventData.expertUuid == expert.uuid

        _ ->
            False


isAddChapter : KnowledgeModel -> Event -> Bool
isAddChapter km event =
    case event of
        AddChapterEvent eventData commonData ->
            case List.last commonData.path of
                Just (KMPathNode uuid) ->
                    uuid == km.uuid

                _ ->
                    False

        _ ->
            False


isAddQuestion : String -> Event -> Bool
isAddQuestion parentUuid event =
    case event of
        AddQuestionEvent eventData commonData ->
            case List.last commonData.path of
                Just (ChapterPathNode uuid) ->
                    uuid == parentUuid

                _ ->
                    False

        _ ->
            False


isAddAnswer : Question -> Event -> Bool
isAddAnswer question event =
    case event of
        AddAnswerEvent eventData commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == question.uuid

                _ ->
                    False

        _ ->
            False


isAddExpert : Question -> Event -> Bool
isAddExpert question event =
    case event of
        AddExpertEvent eventData commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == question.uuid

                _ ->
                    False

        _ ->
            False


isAddReference : Question -> Event -> Bool
isAddReference question event =
    case event of
        AddReferenceEvent eventData commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == question.uuid

                _ ->
                    False

        _ ->
            False
