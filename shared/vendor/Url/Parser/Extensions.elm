module Url.Parser.Extensions exposing (mbString, prefix, uuid)

import String.Extra as String
import Url.Parser exposing ((</>), Parser, custom, map, s)
import Uuid exposing (Uuid)


uuid : Parser (Uuid -> a) a
uuid =
    custom "UUID" Uuid.fromString


prefix : String -> Parser (a -> a) b -> Parser (b -> c) c
prefix prefixString parser =
    map identity (s prefixString </> parser)


mbString : Parser (Maybe String -> a) a
mbString =
    custom "MAYBE_STRING" (Just << String.toMaybe)
