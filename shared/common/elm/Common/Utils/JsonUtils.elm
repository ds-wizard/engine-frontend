module Common.Utils.JsonUtils exposing (prettyString)

import Json.Decode as D
import Json.Encode as E


{-| Pretty-print a JSON string with the given indentation.

Unlike some pretty-printers, this keeps the contents of string values correctly
escaped (e.g. a `"` inside a string stays `\"`), because it re-encodes the
parsed value with `Json.Encode.encode`.

-}
prettyString : Int -> String -> Result String String
prettyString indent json =
    D.decodeString D.value json
        |> Result.map (E.encode indent)
        |> Result.mapError D.errorToString
