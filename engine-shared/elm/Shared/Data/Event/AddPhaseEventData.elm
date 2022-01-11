module Shared.Data.Event.AddPhaseEventData exposing
    ( AddPhaseEventData
    , decoder
    , encode
    , init
    , toPhase
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)


type alias AddPhaseEventData =
    { title : String
    , description : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder AddPhaseEventData
decoder =
    D.succeed AddPhaseEventData
        |> D.required "title" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddPhaseEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddPhaseEvent" )
    , ( "title", E.string data.title )
    , ( "description", E.maybe E.string data.description )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddPhaseEventData
init =
    { title = ""
    , description = Nothing
    , annotations = []
    }


toPhase : String -> AddPhaseEventData -> Phase
toPhase uuid data =
    { uuid = uuid
    , title = data.title
    , description = data.description
    , annotations = data.annotations
    }
