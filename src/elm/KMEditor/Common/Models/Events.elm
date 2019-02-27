module KMEditor.Common.Models.Events exposing
    ( AddAnswerEventData
    , AddChapterEventData
    , AddCrossReferenceEventData
    , AddExpertEventData
    , AddQuestionEventData(..)
    , AddReferenceEventData(..)
    , AddResourcePageReferenceEventData
    , AddTagEventData
    , AddURLReferenceEventData
    , AnswerItemTemplateData
    , CommonEventData
    , DeleteAnswerEventData
    , DeleteChapterEventData
    , DeleteExpertEventData
    , DeleteQuestionEventData
    , DeleteReferenceEventData
    , DeleteTagEventData
    , EditAnswerEventData
    , EditChapterEventData
    , EditCrossReferenceEventData
    , EditExpertEventData
    , EditKnowledgeModelEventData
    , EditListQuestionEventData
    , EditOptionsQuestionEventData
    , EditQuestionEventData(..)
    , EditReferenceEventData(..)
    , EditResourcePageReferenceEventData
    , EditTagEventData
    , EditURLReferenceEventData
    , EditValueQuestionEventData
    , Event(..)
    , EventField
    , createEmptyEventField
    , createEventField
    , encodeEvent
    , encodeEventField
    , encodeEvents
    , eventDecoder
    , eventFieldDecoder
    , getAddQuestionEventEntityVisibleName
    , getAddQuestionUuid
    , getAddReferenceEventEntityVisibleName
    , getAddReferenceUuid
    , getEditQuestionEventEntityVisibleName
    , getEditQuestionUuid
    , getEditReferenceEventEntityVisibleName
    , getEditReferenceUuid
    , getEventEntityUuid
    , getEventEntityVisibleName
    , getEventFieldValue
    , getEventFieldValueWithDefault
    , getEventUuid
    , isAddAnswer
    , isAddChapter
    , isAddExpert
    , isAddQuestion
    , isAddReference
    , isAddTag
    , isDeleteAnswer
    , isDeleteChapter
    , isDeleteExpert
    , isDeleteQuestion
    , isDeleteReference
    , isDeleteTag
    , isEditAnswer
    , isEditChapter
    , isEditExpert
    , isEditQuestion
    , isEditReference
    , isEditTag
    , mapAddQuestionEventData
    , mapAddReferenceEventData
    , mapEditQuestionEventData
    , mapEditReferenceEventData
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Path exposing (..)
import List.Extra as List


type Event
    = AddKnowledgeModelEvent AddKnowledgeModelEventData CommonEventData
    | EditKnowledgeModelEvent EditKnowledgeModelEventData CommonEventData
    | AddChapterEvent AddChapterEventData CommonEventData
    | EditChapterEvent EditChapterEventData CommonEventData
    | DeleteChapterEvent DeleteChapterEventData CommonEventData
    | AddTagEvent AddTagEventData CommonEventData
    | EditTagEvent EditTagEventData CommonEventData
    | DeleteTagEvent DeleteTagEventData CommonEventData
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


type alias AddKnowledgeModelEventData =
    { kmUuid : String
    , name : String
    }


type alias EditKnowledgeModelEventData =
    { kmUuid : String
    , name : EventField String
    , chapterUuids : EventField (List String)
    , tagUuids : EventField (List String)
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
    , questionUuids : EventField (List String)
    }


type alias DeleteChapterEventData =
    { chapterUuid : String
    }


type alias AddTagEventData =
    { tagUuid : String
    , name : String
    , description : Maybe String
    , color : String
    }


type alias EditTagEventData =
    { tagUuid : String
    , name : EventField String
    , description : EventField (Maybe String)
    , color : EventField String
    }


type alias DeleteTagEventData =
    { tagUuid : String
    }


type AddQuestionEventData
    = AddOptionsQuestionEvent AddOptionsQuestionEventData
    | AddListQuestionEvent AddListQuestionEventData
    | AddValueQuestionEvent AddValueQuestionEventData


type alias AddOptionsQuestionEventData =
    { questionUuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    }


type alias AddListQuestionEventData =
    { questionUuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , itemTemplateTitle : String
    }


type alias AddValueQuestionEventData =
    { questionUuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , valueType : ValueQuestionType
    }


type EditQuestionEventData
    = EditOptionsQuestionEvent EditOptionsQuestionEventData
    | EditListQuestionEvent EditListQuestionEventData
    | EditValueQuestionEvent EditValueQuestionEventData


type alias EditOptionsQuestionEventData =
    { questionUuid : String
    , title : EventField String
    , text : EventField (Maybe String)
    , requiredLevel : EventField (Maybe Int)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , answerUuids : EventField (List String)
    }


type alias EditListQuestionEventData =
    { questionUuid : String
    , title : EventField String
    , text : EventField (Maybe String)
    , requiredLevel : EventField (Maybe Int)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , itemTemplateTitle : EventField String
    , itemTemplateQuestionUuids : EventField (List String)
    }


type alias EditValueQuestionEventData =
    { questionUuid : String
    , title : EventField String
    , text : EventField (Maybe String)
    , requiredLevel : EventField (Maybe Int)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , valueType : EventField ValueQuestionType
    }


type alias DeleteQuestionEventData =
    { questionUuid : String
    }


type alias AddAnswerEventData =
    { answerUuid : String
    , label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
    }


type alias EditAnswerEventData =
    { answerUuid : String
    , label : EventField String
    , advice : EventField (Maybe String)
    , metricMeasures : EventField (List MetricMeasure)
    , followUpUuids : EventField (List String)
    }


type alias DeleteAnswerEventData =
    { answerUuid : String
    }


type AddReferenceEventData
    = AddResourcePageReferenceEvent AddResourcePageReferenceEventData
    | AddURLReferenceEvent AddURLReferenceEventData
    | AddCrossReferenceEvent AddCrossReferenceEventData


type alias AddResourcePageReferenceEventData =
    { referenceUuid : String
    , shortUuid : String
    }


type alias AddURLReferenceEventData =
    { referenceUuid : String
    , url : String
    , label : String
    }


type alias AddCrossReferenceEventData =
    { referenceUuid : String
    , targetUuid : String
    , description : String
    }


type EditReferenceEventData
    = EditResourcePageReferenceEvent EditResourcePageReferenceEventData
    | EditURLReferenceEvent EditURLReferenceEventData
    | EditCrossReferenceEvent EditCrossReferenceEventData


type alias EditResourcePageReferenceEventData =
    { referenceUuid : String
    , shortUuid : EventField String
    }


type alias EditURLReferenceEventData =
    { referenceUuid : String
    , url : EventField String
    , label : EventField String
    }


type alias EditCrossReferenceEventData =
    { referenceUuid : String
    , targetUuid : EventField String
    , description : EventField String
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


type alias EventField a =
    { changed : Bool
    , value : Maybe a
    }


type alias AnswerItemTemplateData =
    { title : String
    , questionUuids : List String
    }



{- Encoders -}


encodeEvents : List Event -> Encode.Value
encodeEvents =
    Encode.list encodeEvent


encodeEvent : Event -> Encode.Value
encodeEvent event =
    let
        ( encodedCommonData, encodedEventData ) =
            case event of
                AddKnowledgeModelEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddKnowledgeModelEvent eventData )

                EditKnowledgeModelEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditKnowledgeModelEvent eventData )

                AddChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddChapterEvent eventData )

                EditChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditChapterEvent eventData )

                DeleteChapterEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteChapterEvent eventData )

                AddTagEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeAddTagEvent eventData )

                EditTagEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeEditTagEvent eventData )

                DeleteTagEvent eventData commonData ->
                    ( encodeCommonData commonData, encodeDeleteTagEvent eventData )

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
    Encode.object <| encodedCommonData ++ encodedEventData


encodeCommonData : CommonEventData -> List ( String, Encode.Value )
encodeCommonData data =
    [ ( "uuid", Encode.string data.uuid )
    , ( "path", Encode.list encodePathNode data.path )
    ]


encodeAddKnowledgeModelEvent : AddKnowledgeModelEventData -> List ( String, Encode.Value )
encodeAddKnowledgeModelEvent data =
    [ ( "eventType", Encode.string "AddKnowledgeModelEvent" )
    , ( "kmUuid", Encode.string data.kmUuid )
    , ( "name", Encode.string data.name )
    ]


encodeEditKnowledgeModelEvent : EditKnowledgeModelEventData -> List ( String, Encode.Value )
encodeEditKnowledgeModelEvent data =
    [ ( "eventType", Encode.string "EditKnowledgeModelEvent" )
    , ( "kmUuid", Encode.string data.kmUuid )
    , ( "name", encodeEventField Encode.string data.name )
    , ( "chapterUuids", encodeEventField (Encode.list Encode.string) data.chapterUuids )
    , ( "tagUuids", encodeEventField (Encode.list Encode.string) data.tagUuids )
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
    , ( "questionUuids", encodeEventField (Encode.list Encode.string) data.questionUuids )
    ]


encodeDeleteChapterEvent : DeleteChapterEventData -> List ( String, Encode.Value )
encodeDeleteChapterEvent data =
    [ ( "eventType", Encode.string "DeleteChapterEvent" )
    , ( "chapterUuid", Encode.string data.chapterUuid )
    ]


encodeAddTagEvent : AddTagEventData -> List ( String, Encode.Value )
encodeAddTagEvent data =
    [ ( "eventType", Encode.string "AddTagEvent" )
    , ( "tagUuid", Encode.string data.tagUuid )
    , ( "name", Encode.string data.name )
    , ( "description", maybe Encode.string data.description )
    , ( "color", Encode.string data.color )
    ]


encodeEditTagEvent : EditTagEventData -> List ( String, Encode.Value )
encodeEditTagEvent data =
    [ ( "eventType", Encode.string "EditTagEvent" )
    , ( "tagUuid", Encode.string data.tagUuid )
    , ( "name", encodeEventField Encode.string data.name )
    , ( "description", encodeEventField (maybe Encode.string) data.description )
    , ( "color", encodeEventField Encode.string data.color )
    ]


encodeDeleteTagEvent : DeleteTagEventData -> List ( String, Encode.Value )
encodeDeleteTagEvent data =
    [ ( "eventType", Encode.string "DeleteTagEvent" )
    , ( "tagUuid", Encode.string data.tagUuid )
    ]


encodeAddQuestionEvent : AddQuestionEventData -> List ( String, Encode.Value )
encodeAddQuestionEvent data =
    let
        eventData =
            mapAddQuestionEventData
                encodeAddOptionsQuestionEvent
                encodeAddListQuestionEvent
                encodeAddValueQuestionEvent
                data
    in
    [ ( "eventType", Encode.string "AddQuestionEvent" ) ] ++ eventData


encodeAddOptionsQuestionEvent : AddOptionsQuestionEventData -> List ( String, Encode.Value )
encodeAddOptionsQuestionEvent data =
    [ ( "questionType", Encode.string "OptionsQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", Encode.string data.title )
    , ( "text", maybe Encode.string data.text )
    , ( "requiredLevel", maybe Encode.int data.requiredLevel )
    , ( "tagUuids", Encode.list Encode.string data.tagUuids )
    ]


encodeAddListQuestionEvent : AddListQuestionEventData -> List ( String, Encode.Value )
encodeAddListQuestionEvent data =
    [ ( "questionType", Encode.string "ListQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", Encode.string data.title )
    , ( "text", maybe Encode.string data.text )
    , ( "requiredLevel", maybe Encode.int data.requiredLevel )
    , ( "tagUuids", Encode.list Encode.string data.tagUuids )
    , ( "itemTemplateTitle", Encode.string data.itemTemplateTitle )
    ]


encodeAddValueQuestionEvent : AddValueQuestionEventData -> List ( String, Encode.Value )
encodeAddValueQuestionEvent data =
    [ ( "questionType", Encode.string "ValueQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", Encode.string data.title )
    , ( "text", maybe Encode.string data.text )
    , ( "requiredLevel", maybe Encode.int data.requiredLevel )
    , ( "tagUuids", Encode.list Encode.string data.tagUuids )
    , ( "valueType", encodeValueType data.valueType )
    ]


encodeEditQuestionEvent : EditQuestionEventData -> List ( String, Encode.Value )
encodeEditQuestionEvent data =
    let
        eventData =
            mapEditQuestionEventData
                encodeEditOptionsQuestionEvent
                encodeEditListQuestionEvent
                encodeEditValueQuestionEvent
                data
    in
    [ ( "eventType", Encode.string "EditQuestionEvent" ) ] ++ eventData


encodeEditOptionsQuestionEvent : EditOptionsQuestionEventData -> List ( String, Encode.Value )
encodeEditOptionsQuestionEvent data =
    [ ( "questionType", Encode.string "OptionsQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", encodeEventField Encode.string data.title )
    , ( "text", encodeEventField (maybe Encode.string) data.text )
    , ( "requiredLevel", encodeEventField (maybe Encode.int) data.requiredLevel )
    , ( "tagUuids", encodeEventField (Encode.list Encode.string) data.tagUuids )
    , ( "referenceUuids", encodeEventField (Encode.list Encode.string) data.referenceUuids )
    , ( "expertUuids", encodeEventField (Encode.list Encode.string) data.expertUuids )
    , ( "answerUuids", encodeEventField (Encode.list Encode.string) data.answerUuids )
    ]


encodeEditListQuestionEvent : EditListQuestionEventData -> List ( String, Encode.Value )
encodeEditListQuestionEvent data =
    [ ( "questionType", Encode.string "ListQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", encodeEventField Encode.string data.title )
    , ( "text", encodeEventField (maybe Encode.string) data.text )
    , ( "requiredLevel", encodeEventField (maybe Encode.int) data.requiredLevel )
    , ( "tagUuids", encodeEventField (Encode.list Encode.string) data.tagUuids )
    , ( "referenceUuids", encodeEventField (Encode.list Encode.string) data.referenceUuids )
    , ( "expertUuids", encodeEventField (Encode.list Encode.string) data.expertUuids )
    , ( "itemTemplateTitle", encodeEventField Encode.string data.itemTemplateTitle )
    , ( "itemTemplateQuestionUuids", encodeEventField (Encode.list Encode.string) data.itemTemplateQuestionUuids )
    ]


encodeEditValueQuestionEvent : EditValueQuestionEventData -> List ( String, Encode.Value )
encodeEditValueQuestionEvent data =
    [ ( "questionType", Encode.string "ValueQuestion" )
    , ( "questionUuid", Encode.string data.questionUuid )
    , ( "title", encodeEventField Encode.string data.title )
    , ( "text", encodeEventField (maybe Encode.string) data.text )
    , ( "requiredLevel", encodeEventField (maybe Encode.int) data.requiredLevel )
    , ( "tagUuids", encodeEventField (Encode.list Encode.string) data.tagUuids )
    , ( "referenceUuids", encodeEventField (Encode.list Encode.string) data.referenceUuids )
    , ( "expertUuids", encodeEventField (Encode.list Encode.string) data.expertUuids )
    , ( "valueType", encodeEventField encodeValueType data.valueType )
    ]


encodeDeleteQuestionEvent : DeleteQuestionEventData -> List ( String, Encode.Value )
encodeDeleteQuestionEvent data =
    [ ( "eventType", Encode.string "DeleteQuestionEvent" )
    , ( "questionUuid", Encode.string data.questionUuid )
    ]


encodeValueType : ValueQuestionType -> Encode.Value
encodeValueType valueType =
    Encode.string <|
        case valueType of
            StringValueType ->
                "StringValue"

            DateValueType ->
                "DateValue"

            NumberValueType ->
                "NumberValue"

            TextValueType ->
                "TextValue"


encodeAddAnswerEvent : AddAnswerEventData -> List ( String, Encode.Value )
encodeAddAnswerEvent data =
    [ ( "eventType", Encode.string "AddAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    , ( "label", Encode.string data.label )
    , ( "advice", maybe Encode.string data.advice )
    , ( "metricMeasures", Encode.list metricMeasureEncoder data.metricMeasures )
    ]


encodeEditAnswerEvent : EditAnswerEventData -> List ( String, Encode.Value )
encodeEditAnswerEvent data =
    [ ( "eventType", Encode.string "EditAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    , ( "label", encodeEventField Encode.string data.label )
    , ( "advice", encodeEventField (maybe Encode.string) data.advice )
    , ( "metricMeasures", encodeEventField (Encode.list metricMeasureEncoder) data.metricMeasures )
    , ( "followUpUuids", encodeEventField (Encode.list Encode.string) data.followUpUuids )
    ]


encodeDeleteAnswerEvent : DeleteAnswerEventData -> List ( String, Encode.Value )
encodeDeleteAnswerEvent data =
    [ ( "eventType", Encode.string "DeleteAnswerEvent" )
    , ( "answerUuid", Encode.string data.answerUuid )
    ]


encodeAddReferenceEvent : AddReferenceEventData -> List ( String, Encode.Value )
encodeAddReferenceEvent data =
    let
        eventData =
            mapAddReferenceEventData
                encodeAddResourcePageReferenceEvent
                encodeAddURLReferenceEvent
                encodeAddCrossReferenceEvent
                data
    in
    [ ( "eventType", Encode.string "AddReferenceEvent" ) ] ++ eventData


encodeAddResourcePageReferenceEvent : AddResourcePageReferenceEventData -> List ( String, Encode.Value )
encodeAddResourcePageReferenceEvent data =
    [ ( "referenceType", Encode.string "ResourcePageReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "shortUuid", Encode.string data.shortUuid )
    ]


encodeAddURLReferenceEvent : AddURLReferenceEventData -> List ( String, Encode.Value )
encodeAddURLReferenceEvent data =
    [ ( "referenceType", Encode.string "URLReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "url", Encode.string data.url )
    , ( "label", Encode.string data.label )
    ]


encodeAddCrossReferenceEvent : AddCrossReferenceEventData -> List ( String, Encode.Value )
encodeAddCrossReferenceEvent data =
    [ ( "referenceType", Encode.string "CrossReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "targetUuid", Encode.string data.targetUuid )
    , ( "description", Encode.string data.description )
    ]


encodeEditReferenceEvent : EditReferenceEventData -> List ( String, Encode.Value )
encodeEditReferenceEvent data =
    let
        eventData =
            mapEditReferenceEventData
                encodeEditResourcePageReferenceEvent
                encodeEditURLReferenceEvent
                encodeEditCrossReferenceEvent
                data
    in
    [ ( "eventType", Encode.string "EditReferenceEvent" ) ] ++ eventData


encodeEditResourcePageReferenceEvent : EditResourcePageReferenceEventData -> List ( String, Encode.Value )
encodeEditResourcePageReferenceEvent data =
    [ ( "referenceType", Encode.string "ResourcePageReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "shortUuid", encodeEventField Encode.string data.shortUuid )
    ]


encodeEditURLReferenceEvent : EditURLReferenceEventData -> List ( String, Encode.Value )
encodeEditURLReferenceEvent data =
    [ ( "referenceType", Encode.string "URLReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "url", encodeEventField Encode.string data.url )
    , ( "label", encodeEventField Encode.string data.label )
    ]


encodeEditCrossReferenceEvent : EditCrossReferenceEventData -> List ( String, Encode.Value )
encodeEditCrossReferenceEvent data =
    [ ( "referenceType", Encode.string "CrossReference" )
    , ( "referenceUuid", Encode.string data.referenceUuid )
    , ( "targetUuid", encodeEventField Encode.string data.targetUuid )
    , ( "description", encodeEventField Encode.string data.description )
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



{- Decoders -}


eventDecoder : Decoder Event
eventDecoder =
    Decode.field "eventType" Decode.string
        |> Decode.andThen eventDecoderByType


eventDecoderByType : String -> Decoder Event
eventDecoderByType eventType =
    case eventType of
        "AddKnowledgeModelEvent" ->
            Decode.map2 AddKnowledgeModelEvent addKnowledgeModelEventDecoder commonEventDataDecoder

        "EditKnowledgeModelEvent" ->
            Decode.map2 EditKnowledgeModelEvent editKnowledgeModelEventDecoder commonEventDataDecoder

        "AddChapterEvent" ->
            Decode.map2 AddChapterEvent addChapterEventDecoder commonEventDataDecoder

        "EditChapterEvent" ->
            Decode.map2 EditChapterEvent editChapterEventDecoder commonEventDataDecoder

        "DeleteChapterEvent" ->
            Decode.map2 DeleteChapterEvent deleteChapterEventDecoder commonEventDataDecoder

        "AddTagEvent" ->
            Decode.map2 AddTagEvent addTagEventDecoder commonEventDataDecoder

        "EditTagEvent" ->
            Decode.map2 EditTagEvent editTagEventDecoder commonEventDataDecoder

        "DeleteTagEvent" ->
            Decode.map2 DeleteTagEvent deleteTagEventDecoder commonEventDataDecoder

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
    Decode.succeed CommonEventData
        |> required "uuid" Decode.string
        |> required "path" pathDecoder


addKnowledgeModelEventDecoder : Decoder AddKnowledgeModelEventData
addKnowledgeModelEventDecoder =
    Decode.succeed AddKnowledgeModelEventData
        |> required "kmUuid" Decode.string
        |> required "name" Decode.string


editKnowledgeModelEventDecoder : Decoder EditKnowledgeModelEventData
editKnowledgeModelEventDecoder =
    Decode.succeed EditKnowledgeModelEventData
        |> required "kmUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "chapterUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "tagUuids" (eventFieldDecoder (Decode.list Decode.string))


addChapterEventDecoder : Decoder AddChapterEventData
addChapterEventDecoder =
    Decode.succeed AddChapterEventData
        |> required "chapterUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string


editChapterEventDecoder : Decoder EditChapterEventData
editChapterEventDecoder =
    Decode.succeed EditChapterEventData
        |> required "chapterUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder Decode.string)
        |> required "questionUuids" (eventFieldDecoder (Decode.list Decode.string))


deleteChapterEventDecoder : Decoder DeleteChapterEventData
deleteChapterEventDecoder =
    Decode.succeed DeleteChapterEventData
        |> required "chapterUuid" Decode.string


addTagEventDecoder : Decoder AddTagEventData
addTagEventDecoder =
    Decode.succeed AddTagEventData
        |> required "tagUuid" Decode.string
        |> required "name" Decode.string
        |> required "description" (Decode.nullable Decode.string)
        |> required "color" Decode.string


editTagEventDecoder : Decoder EditTagEventData
editTagEventDecoder =
    Decode.succeed EditTagEventData
        |> required "tagUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "description" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "color" (eventFieldDecoder Decode.string)


deleteTagEventDecoder : Decoder DeleteTagEventData
deleteTagEventDecoder =
    Decode.succeed DeleteTagEventData
        |> required "tagUuid" Decode.string


addQuestionEventDecoder : Decoder AddQuestionEventData
addQuestionEventDecoder =
    Decode.field "questionType" Decode.string
        |> Decode.andThen addQuestionEventDecoderByType


addQuestionEventDecoderByType : String -> Decoder AddQuestionEventData
addQuestionEventDecoderByType questionType =
    case questionType of
        "OptionsQuestion" ->
            Decode.map AddOptionsQuestionEvent addOptionsQuestionEventDecoder

        "ListQuestion" ->
            Decode.map AddListQuestionEvent addListQuestionEventDecoder

        "ValueQuestion" ->
            Decode.map AddValueQuestionEvent addValueQuestionEventDecoder

        _ ->
            Decode.fail <| "Unknown question type: " ++ questionType


addOptionsQuestionEventDecoder : Decoder AddOptionsQuestionEventData
addOptionsQuestionEventDecoder =
    Decode.succeed AddOptionsQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)


addListQuestionEventDecoder : Decoder AddListQuestionEventData
addListQuestionEventDecoder =
    Decode.succeed AddListQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)
        |> required "itemTemplateTitle" Decode.string


addValueQuestionEventDecoder : Decoder AddValueQuestionEventData
addValueQuestionEventDecoder =
    Decode.succeed AddValueQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)
        |> required "valueType" valueTypeDecoder


editQuestionEventDecoder : Decoder EditQuestionEventData
editQuestionEventDecoder =
    Decode.field "questionType" Decode.string
        |> Decode.andThen editQuestionEventDecoderByType


editQuestionEventDecoderByType : String -> Decoder EditQuestionEventData
editQuestionEventDecoderByType questionType =
    case questionType of
        "OptionsQuestion" ->
            Decode.map EditOptionsQuestionEvent editOptionsQuestionEventDecoder

        "ListQuestion" ->
            Decode.map EditListQuestionEvent editListQuestionEventDecoder

        "ValueQuestion" ->
            Decode.map EditValueQuestionEvent editValueQuestionEventDecoder

        _ ->
            Decode.fail <| "Unknown question type: " ++ questionType


editOptionsQuestionEventDecoder : Decoder EditOptionsQuestionEventData
editOptionsQuestionEventDecoder =
    Decode.succeed EditOptionsQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "requiredLevel" (eventFieldDecoder (Decode.nullable Decode.int))
        |> required "tagUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "expertUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "answerUuids" (eventFieldDecoder (Decode.list Decode.string))


editListQuestionEventDecoder : Decoder EditListQuestionEventData
editListQuestionEventDecoder =
    Decode.succeed EditListQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "requiredLevel" (eventFieldDecoder (Decode.nullable Decode.int))
        |> required "tagUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "expertUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "itemTemplateTitle" (eventFieldDecoder Decode.string)
        |> required "itemTemplateQuestionUuids" (eventFieldDecoder (Decode.list Decode.string))


editValueQuestionEventDecoder : Decoder EditValueQuestionEventData
editValueQuestionEventDecoder =
    Decode.succeed EditValueQuestionEventData
        |> required "questionUuid" Decode.string
        |> required "title" (eventFieldDecoder Decode.string)
        |> required "text" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "requiredLevel" (eventFieldDecoder (Decode.nullable Decode.int))
        |> required "tagUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "referenceUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "expertUuids" (eventFieldDecoder (Decode.list Decode.string))
        |> required "valueType" (eventFieldDecoder valueTypeDecoder)


deleteQuestionEventDecoder : Decoder DeleteQuestionEventData
deleteQuestionEventDecoder =
    Decode.succeed DeleteQuestionEventData
        |> required "questionUuid" Decode.string


addAnswerEventDecoder : Decoder AddAnswerEventData
addAnswerEventDecoder =
    Decode.succeed AddAnswerEventData
        |> required "answerUuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> required "metricMeasures" (Decode.list metricMeasureDecoder)


editAnswerEventDecoder : Decoder EditAnswerEventData
editAnswerEventDecoder =
    Decode.succeed EditAnswerEventData
        |> required "answerUuid" Decode.string
        |> required "label" (eventFieldDecoder Decode.string)
        |> required "advice" (eventFieldDecoder (Decode.nullable Decode.string))
        |> required "metricMeasures" (eventFieldDecoder (Decode.list metricMeasureDecoder))
        |> required "followUpUuids" (eventFieldDecoder (Decode.list Decode.string))


deleteAnswerEventDecoder : Decoder DeleteAnswerEventData
deleteAnswerEventDecoder =
    Decode.succeed DeleteAnswerEventData
        |> required "answerUuid" Decode.string


addReferenceEventDecoder : Decoder AddReferenceEventData
addReferenceEventDecoder =
    Decode.field "referenceType" Decode.string
        |> Decode.andThen addReferenceEventDecoderByType


addReferenceEventDecoderByType : String -> Decoder AddReferenceEventData
addReferenceEventDecoderByType referenceType =
    case referenceType of
        "ResourcePageReference" ->
            Decode.map AddResourcePageReferenceEvent addResourcePageReferenceEventDecoder

        "URLReference" ->
            Decode.map AddURLReferenceEvent addURLReferenceEventDecoder

        "CrossReference" ->
            Decode.map AddCrossReferenceEvent addCrossReferenceEventDecoder

        _ ->
            Decode.fail <| "Unknown reference type: " ++ referenceType


addResourcePageReferenceEventDecoder : Decoder AddResourcePageReferenceEventData
addResourcePageReferenceEventDecoder =
    Decode.succeed AddResourcePageReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "shortUuid" Decode.string


addURLReferenceEventDecoder : Decoder AddURLReferenceEventData
addURLReferenceEventDecoder =
    Decode.succeed AddURLReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "url" Decode.string
        |> required "label" Decode.string


addCrossReferenceEventDecoder : Decoder AddCrossReferenceEventData
addCrossReferenceEventDecoder =
    Decode.succeed AddCrossReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "targetUuid" Decode.string
        |> required "description" Decode.string


editReferenceEventDecoder : Decoder EditReferenceEventData
editReferenceEventDecoder =
    Decode.field "referenceType" Decode.string
        |> Decode.andThen editReferenceEventDecoderByType


editReferenceEventDecoderByType : String -> Decoder EditReferenceEventData
editReferenceEventDecoderByType referenceType =
    case referenceType of
        "ResourcePageReference" ->
            Decode.map EditResourcePageReferenceEvent editResourcePageReferenceEventDecoder

        "URLReference" ->
            Decode.map EditURLReferenceEvent editURLReferenceEventDecoder

        "CrossReference" ->
            Decode.map EditCrossReferenceEvent editCrossReferenceEventDecoder

        _ ->
            Decode.fail <| "Unknown reference type: " ++ referenceType


editResourcePageReferenceEventDecoder : Decoder EditResourcePageReferenceEventData
editResourcePageReferenceEventDecoder =
    Decode.succeed EditResourcePageReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "shortUuid" (eventFieldDecoder Decode.string)


editURLReferenceEventDecoder : Decoder EditURLReferenceEventData
editURLReferenceEventDecoder =
    Decode.succeed EditURLReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "url" (eventFieldDecoder Decode.string)
        |> required "label" (eventFieldDecoder Decode.string)


editCrossReferenceEventDecoder : Decoder EditCrossReferenceEventData
editCrossReferenceEventDecoder =
    Decode.succeed EditCrossReferenceEventData
        |> required "referenceUuid" Decode.string
        |> required "targetUuid" (eventFieldDecoder Decode.string)
        |> required "description" (eventFieldDecoder Decode.string)


deleteReferenceEventDecoder : Decoder DeleteReferenceEventData
deleteReferenceEventDecoder =
    Decode.succeed DeleteReferenceEventData
        |> required "referenceUuid" Decode.string


addExpertEventDecoder : Decoder AddExpertEventData
addExpertEventDecoder =
    Decode.succeed AddExpertEventData
        |> required "expertUuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string


editExpertEventDecoder : Decoder EditExpertEventData
editExpertEventDecoder =
    Decode.succeed EditExpertEventData
        |> required "expertUuid" Decode.string
        |> required "name" (eventFieldDecoder Decode.string)
        |> required "email" (eventFieldDecoder Decode.string)


deleteExpertEventDecoder : Decoder DeleteExpertEventData
deleteExpertEventDecoder =
    Decode.succeed DeleteExpertEventData
        |> required "expertUuid" Decode.string


eventFieldDecoder : Decoder a -> Decoder (EventField a)
eventFieldDecoder decoder =
    Decode.succeed EventField
        |> required "changed" Decode.bool
        |> optional "value" (Decode.nullable decoder) Nothing



{- Helpers -}


getEventUuid : Event -> String
getEventUuid event =
    case event of
        AddKnowledgeModelEvent _ commonData ->
            commonData.uuid

        EditKnowledgeModelEvent _ commonData ->
            commonData.uuid

        AddTagEvent _ commonData ->
            commonData.uuid

        EditTagEvent _ commonData ->
            commonData.uuid

        DeleteTagEvent _ commonData ->
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


getEventEntityUuid : Event -> String
getEventEntityUuid event =
    case event of
        AddKnowledgeModelEvent eventData _ ->
            eventData.kmUuid

        EditKnowledgeModelEvent eventData _ ->
            eventData.kmUuid

        AddTagEvent eventData _ ->
            eventData.tagUuid

        EditTagEvent eventData _ ->
            eventData.tagUuid

        DeleteTagEvent eventData _ ->
            eventData.tagUuid

        AddChapterEvent eventData _ ->
            eventData.chapterUuid

        EditChapterEvent eventData _ ->
            eventData.chapterUuid

        DeleteChapterEvent eventData _ ->
            eventData.chapterUuid

        AddQuestionEvent eventData _ ->
            getAddQuestionUuid eventData

        EditQuestionEvent eventData _ ->
            getEditQuestionUuid eventData

        DeleteQuestionEvent eventData _ ->
            eventData.questionUuid

        AddAnswerEvent eventData _ ->
            eventData.answerUuid

        EditAnswerEvent eventData _ ->
            eventData.answerUuid

        DeleteAnswerEvent eventData _ ->
            eventData.answerUuid

        AddReferenceEvent eventData _ ->
            getAddReferenceUuid eventData

        EditReferenceEvent eventData _ ->
            getEditReferenceUuid eventData

        DeleteReferenceEvent eventData _ ->
            eventData.referenceUuid

        AddExpertEvent eventData _ ->
            eventData.expertUuid

        EditExpertEvent eventData _ ->
            eventData.expertUuid

        DeleteExpertEvent eventData _ ->
            eventData.expertUuid


getEventFieldValue : EventField a -> Maybe a
getEventFieldValue eventField =
    if eventField.changed then
        eventField.value

    else
        Nothing


getEventFieldValueWithDefault : EventField a -> a -> a
getEventFieldValueWithDefault eventField default =
    getEventFieldValue eventField |> Maybe.withDefault default


createEventField : a -> Bool -> EventField a
createEventField value changed =
    let
        v =
            if changed then
                Just value

            else
                Nothing
    in
    { changed = changed
    , value = v
    }


createEmptyEventField : EventField a
createEmptyEventField =
    { changed = False
    , value = Nothing
    }


getEventEntityVisibleName : Event -> Maybe String
getEventEntityVisibleName event =
    case event of
        AddKnowledgeModelEvent eventData _ ->
            Just eventData.name

        EditKnowledgeModelEvent eventData _ ->
            getEventFieldValue eventData.name

        AddTagEvent eventData _ ->
            Just eventData.name

        EditTagEvent eventData _ ->
            getEventFieldValue eventData.name

        AddChapterEvent eventData _ ->
            Just eventData.title

        EditChapterEvent eventData _ ->
            getEventFieldValue eventData.title

        AddQuestionEvent eventData _ ->
            getAddQuestionEventEntityVisibleName eventData

        EditQuestionEvent eventData _ ->
            getEditQuestionEventEntityVisibleName eventData

        AddAnswerEvent eventData _ ->
            Just eventData.label

        EditAnswerEvent eventData _ ->
            getEventFieldValue eventData.label

        AddReferenceEvent eventData _ ->
            getAddReferenceEventEntityVisibleName eventData

        EditReferenceEvent eventData _ ->
            getEditReferenceEventEntityVisibleName eventData

        AddExpertEvent eventData _ ->
            Just eventData.name

        EditExpertEvent eventData _ ->
            getEventFieldValue eventData.name

        _ ->
            Nothing


getAddQuestionEventEntityVisibleName : AddQuestionEventData -> Maybe String
getAddQuestionEventEntityVisibleName =
    Just << mapAddQuestionEventData .title .title .title


getEditQuestionEventEntityVisibleName : EditQuestionEventData -> Maybe String
getEditQuestionEventEntityVisibleName =
    getEventFieldValue << mapEditQuestionEventData .title .title .title


getAddReferenceEventEntityVisibleName : AddReferenceEventData -> Maybe String
getAddReferenceEventEntityVisibleName =
    Just << mapAddReferenceEventData .shortUuid .label .targetUuid


getEditReferenceEventEntityVisibleName : EditReferenceEventData -> Maybe String
getEditReferenceEventEntityVisibleName =
    getEventFieldValue << mapEditReferenceEventData .shortUuid .label .targetUuid


getAddQuestionUuid : AddQuestionEventData -> String
getAddQuestionUuid =
    mapAddQuestionEventData .questionUuid .questionUuid .questionUuid


getEditQuestionUuid : EditQuestionEventData -> String
getEditQuestionUuid =
    mapEditQuestionEventData .questionUuid .questionUuid .questionUuid


getAddReferenceUuid : AddReferenceEventData -> String
getAddReferenceUuid =
    mapAddReferenceEventData .referenceUuid .referenceUuid .referenceUuid


getEditReferenceUuid : EditReferenceEventData -> String
getEditReferenceUuid =
    mapEditReferenceEventData .referenceUuid .referenceUuid .referenceUuid


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


isEditTag : Tag -> Event -> Bool
isEditTag tag event =
    case event of
        EditTagEvent eventData _ ->
            eventData.tagUuid == tag.uuid

        _ ->
            False


isDeleteTag : Tag -> Event -> Bool
isDeleteTag tag event =
    case event of
        DeleteTagEvent eventData _ ->
            eventData.tagUuid == tag.uuid

        _ ->
            False


isEditQuestion : Question -> Event -> Bool
isEditQuestion question event =
    case event of
        EditQuestionEvent eventData _ ->
            getEditQuestionUuid eventData == getQuestionUuid question

        _ ->
            False


isDeleteQuestion : Question -> Event -> Bool
isDeleteQuestion question event =
    case event of
        DeleteQuestionEvent eventData _ ->
            eventData.questionUuid == getQuestionUuid question

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
            getEditReferenceUuid eventData == getReferenceUuid reference

        _ ->
            False


isDeleteReference : Reference -> Event -> Bool
isDeleteReference reference event =
    case event of
        DeleteReferenceEvent eventData _ ->
            eventData.referenceUuid == getReferenceUuid reference

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
        AddChapterEvent _ commonData ->
            case List.last commonData.path of
                Just (KMPathNode uuid) ->
                    uuid == km.uuid

                _ ->
                    False

        _ ->
            False


isAddTag : KnowledgeModel -> Event -> Bool
isAddTag km event =
    case event of
        AddTagEvent _ commonData ->
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
        AddQuestionEvent _ commonData ->
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
        AddAnswerEvent _ commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == getQuestionUuid question

                _ ->
                    False

        _ ->
            False


isAddExpert : Question -> Event -> Bool
isAddExpert question event =
    case event of
        AddExpertEvent _ commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == getQuestionUuid question

                _ ->
                    False

        _ ->
            False


isAddReference : Question -> Event -> Bool
isAddReference question event =
    case event of
        AddReferenceEvent _ commonData ->
            case List.last commonData.path of
                Just (QuestionPathNode uuid) ->
                    uuid == getQuestionUuid question

                _ ->
                    False

        _ ->
            False


mapAddQuestionEventData : (AddOptionsQuestionEventData -> a) -> (AddListQuestionEventData -> a) -> (AddValueQuestionEventData -> a) -> AddQuestionEventData -> a
mapAddQuestionEventData optionsQuestion listQuestion valueQuestion question =
    case question of
        AddOptionsQuestionEvent data ->
            optionsQuestion data

        AddListQuestionEvent data ->
            listQuestion data

        AddValueQuestionEvent data ->
            valueQuestion data


mapEditQuestionEventData : (EditOptionsQuestionEventData -> a) -> (EditListQuestionEventData -> a) -> (EditValueQuestionEventData -> a) -> EditQuestionEventData -> a
mapEditQuestionEventData optionsQuestion listQuestion valueQuestion question =
    case question of
        EditOptionsQuestionEvent data ->
            optionsQuestion data

        EditListQuestionEvent data ->
            listQuestion data

        EditValueQuestionEvent data ->
            valueQuestion data


mapAddReferenceEventData : (AddResourcePageReferenceEventData -> a) -> (AddURLReferenceEventData -> a) -> (AddCrossReferenceEventData -> a) -> AddReferenceEventData -> a
mapAddReferenceEventData resourcePageReference urlReference crossReference reference =
    case reference of
        AddResourcePageReferenceEvent data ->
            resourcePageReference data

        AddURLReferenceEvent data ->
            urlReference data

        AddCrossReferenceEvent data ->
            crossReference data


mapEditReferenceEventData : (EditResourcePageReferenceEventData -> a) -> (EditURLReferenceEventData -> a) -> (EditCrossReferenceEventData -> a) -> EditReferenceEventData -> a
mapEditReferenceEventData resourcePageReference urlReference crossReference reference =
    case reference of
        EditResourcePageReferenceEvent data ->
            resourcePageReference data

        EditURLReferenceEvent data ->
            urlReference data

        EditCrossReferenceEvent data ->
            crossReference data
