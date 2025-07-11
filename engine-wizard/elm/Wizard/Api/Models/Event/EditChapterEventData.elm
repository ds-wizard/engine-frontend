module Wizard.Api.Models.Event.EditChapterEventData exposing
    ( EditChapterEventData
    , apply
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)


type alias EditChapterEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , questionUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditChapterEventData
decoder =
    D.succeed EditChapterEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "questionUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditChapterEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "questionUuids", EventField.encode (E.list E.string) data.questionUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditChapterEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , questionUuids = EventField.empty
    , annotations = EventField.empty
    }


apply : EditChapterEventData -> Chapter -> Chapter
apply eventData chapter =
    { chapter
        | title = EventField.getValueWithDefault eventData.title chapter.title
        , text = EventField.getValueWithDefault eventData.text chapter.text
        , questionUuids = EventField.applyChildren eventData.questionUuids chapter.questionUuids
        , annotations = EventField.getValueWithDefault eventData.annotations chapter.annotations
    }


squash : EditChapterEventData -> EditChapterEventData -> EditChapterEventData
squash oldData newData =
    { title = EventField.squash oldData.title newData.title
    , text = EventField.squash oldData.text newData.text
    , questionUuids = EventField.squash oldData.questionUuids newData.questionUuids
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
