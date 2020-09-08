module Shared.Data.WebSockets.QuestionnaireAction.SetLevelData exposing (SetLevelData, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias SetLevelData =
    { uuid : Uuid
    , level : Int
    }


encode : SetLevelData -> E.Value
encode data =
    E.object
        [ ( "uuid", Uuid.encode data.uuid )
        , ( "level", E.int data.level )
        ]


decoder : Decoder SetLevelData
decoder =
    D.succeed SetLevelData
        |> D.required "uuid" Uuid.decoder
        |> D.required "level" D.int
