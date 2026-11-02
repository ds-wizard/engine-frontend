module Json.Encode.Extensions exposing (stringToValue)

import Json.Decode as D
import Json.Encode as E


stringToValue : String -> E.Value
stringToValue str =
    case D.decodeString D.value str of
        Ok val ->
            val

        Err _ ->
            E.null
