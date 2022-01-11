module Shared.Data.Event.AddTagEventData exposing
    ( AddTagEventData
    , decoder
    , encode
    , init
    , toTag
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)


type alias AddTagEventData =
    { name : String
    , description : Maybe String
    , color : String
    , annotations : List Annotation
    }


decoder : Decoder AddTagEventData
decoder =
    D.succeed AddTagEventData
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddTagEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddTagEvent" )
    , ( "name", E.string data.name )
    , ( "description", E.maybe E.string data.description )
    , ( "color", E.string data.color )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddTagEventData
init =
    { name = ""
    , description = Nothing
    , color = ""
    , annotations = []
    }


toTag : String -> AddTagEventData -> Tag
toTag uuid data =
    { uuid = uuid
    , name = data.name
    , description = data.description
    , color = data.color
    , annotations = data.annotations
    }
