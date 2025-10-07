module Wizard.Api.Models.Event.EditTagEventData exposing
    ( EditTagEventData
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
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)


type alias EditTagEventData =
    { name : EventField String
    , description : EventField (Maybe String)
    , color : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditTagEventData
decoder =
    D.succeed EditTagEventData
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "description" (EventField.decoder (D.nullable D.string))
        |> D.required "color" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditTagEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditTagEvent" )
    , ( "name", EventField.encode E.string data.name )
    , ( "description", EventField.encode (E.maybe E.string) data.description )
    , ( "color", EventField.encode E.string data.color )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditTagEventData
init =
    { name = EventField.empty
    , description = EventField.empty
    , color = EventField.empty
    , annotations = EventField.empty
    }


apply : EditTagEventData -> Tag -> Tag
apply eventData tag =
    { tag
        | name = EventField.getValueWithDefault eventData.name tag.name
        , description = EventField.getValueWithDefault eventData.description tag.description
        , color = EventField.getValueWithDefault eventData.color tag.color
        , annotations = EventField.getValueWithDefault eventData.annotations tag.annotations
    }


squash : EditTagEventData -> EditTagEventData -> EditTagEventData
squash oldData newData =
    { name = EventField.squash oldData.name newData.name
    , description = EventField.squash oldData.description newData.description
    , color = EventField.squash oldData.color newData.color
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
