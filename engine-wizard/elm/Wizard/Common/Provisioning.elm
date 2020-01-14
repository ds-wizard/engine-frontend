module Wizard.Common.Provisioning exposing (Provisioning, decoder, default, merge)

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


merge : Provisioning -> Provisioning -> Provisioning
merge provisioningA provisioningB =
    let
        dictMerge =
            Dict.merge
                (\key a -> Dict.insert key a)
                (\key _ b -> Dict.insert key b)
                (\key b -> Dict.insert key b)
    in
    { locale = dictMerge provisioningA.locale provisioningB.locale Dict.empty
    , iconSet = dictMerge provisioningA.iconSet provisioningB.iconSet Dict.empty
    }
