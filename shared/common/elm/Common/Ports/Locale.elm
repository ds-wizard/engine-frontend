port module Common.Ports.Locale exposing
    ( convertLocaleFile
    , localeConverted
    )

import Json.Decode as D
import Json.Encode as E


port convertLocaleFile : E.Value -> Cmd msg


port localeConverted : (D.Value -> msg) -> Sub msg
