module Wizard.Api.Models.KnowledgeModel.Annotation exposing
    ( Annotation
    , decoder
    , encode
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Annotation =
    { key : String
    , value : String
    }


decoder : Decoder Annotation
decoder =
    D.succeed Annotation
        |> D.required "key" D.string
        |> D.required "value" D.string


encode : Annotation -> E.Value
encode annotation =
    E.object
        [ ( "key", E.string annotation.key )
        , ( "value", E.string annotation.value )
        ]


new : Annotation
new =
    { key = ""
    , value = ""
    }
