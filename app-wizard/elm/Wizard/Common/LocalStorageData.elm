module Wizard.Common.LocalStorageData exposing
    ( LocalStorageData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias LocalStorageData a =
    { key : String
    , value : a
    }


decoder : Decoder a -> Decoder (LocalStorageData a)
decoder valueDecoder =
    D.succeed LocalStorageData
        |> D.required "key" D.string
        |> D.required "value" valueDecoder


encode : (a -> E.Value) -> LocalStorageData a -> E.Value
encode encodeValue data =
    E.object
        [ ( "key", E.string data.key )
        , ( "value", encodeValue data.value )
        ]
