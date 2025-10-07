module Wizard.Api.Models.Event.EditKnowledgeModelEventData exposing
    ( EditKnowledgeModelEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditKnowledgeModelEventData =
    { chapterUuids : EventField (List String)
    , metricUuids : EventField (List String)
    , phaseUuids : EventField (List String)
    , tagUuids : EventField (List String)
    , integrationUuids : EventField (List String)
    , resourceCollectionUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditKnowledgeModelEventData
decoder =
    D.succeed EditKnowledgeModelEventData
        |> D.required "chapterUuids" (EventField.decoder (D.list D.string))
        |> D.required "metricUuids" (EventField.decoder (D.list D.string))
        |> D.required "phaseUuids" (EventField.decoder (D.list D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "integrationUuids" (EventField.decoder (D.list D.string))
        |> D.required "resourceCollectionUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditKnowledgeModelEvent" )
    , ( "chapterUuids", EventField.encode (E.list E.string) data.chapterUuids )
    , ( "metricUuids", EventField.encode (E.list E.string) data.metricUuids )
    , ( "phaseUuids", EventField.encode (E.list E.string) data.phaseUuids )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "integrationUuids", EventField.encode (E.list E.string) data.integrationUuids )
    , ( "resourceCollectionUuids", EventField.encode (E.list E.string) data.resourceCollectionUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditKnowledgeModelEventData
init =
    { chapterUuids = EventField.empty
    , metricUuids = EventField.empty
    , phaseUuids = EventField.empty
    , tagUuids = EventField.empty
    , integrationUuids = EventField.empty
    , resourceCollectionUuids = EventField.empty
    , annotations = EventField.empty
    }


apply : EditKnowledgeModelEventData -> KnowledgeModel -> KnowledgeModel
apply eventData km =
    { km
        | chapterUuids = EventField.applyChildren eventData.chapterUuids km.chapterUuids
        , metricUuids = EventField.applyChildren eventData.metricUuids km.metricUuids
        , phaseUuids = EventField.applyChildren eventData.phaseUuids km.phaseUuids
        , tagUuids = EventField.applyChildren eventData.tagUuids km.tagUuids
        , integrationUuids = EventField.applyChildren eventData.integrationUuids km.integrationUuids
        , resourceCollectionUuids = EventField.applyChildren eventData.resourceCollectionUuids km.resourceCollectionUuids
        , annotations = EventField.getValueWithDefault eventData.annotations km.annotations
    }
