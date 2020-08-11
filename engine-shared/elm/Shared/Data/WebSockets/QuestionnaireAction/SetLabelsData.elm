module Shared.Data.WebSockets.QuestionnaireAction.SetLabelsData exposing
    ( SetLabelsData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias SetLabelsData =
    { uuid : Uuid
    , path : String
    , value : List String
    }


encode : SetLabelsData -> E.Value
encode data =
    E.object
        [ ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "value", E.list E.string data.value )
        ]


decoder : Decoder SetLabelsData
decoder =
    D.succeed SetLabelsData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "value" (D.list D.string)
