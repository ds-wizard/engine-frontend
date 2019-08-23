module Common.Provisioning exposing (Provisioning, decoder, default)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Provisioning =
    { locale : Dict String String
    , iconSet : Dict String String
    }


default : Provisioning
default =
    { locale = Dict.empty
    , iconSet = Dict.empty
    }


decoder : Decoder Provisioning
decoder =
    D.succeed Provisioning
        |> D.required "locale" (D.dict D.string)
        |> D.required "iconSet" (D.dict D.string)
