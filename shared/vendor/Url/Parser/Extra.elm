module Url.Parser.Extra exposing (uuid)

import Url.Parser exposing (Parser, custom)
import Uuid exposing (Uuid)


uuid : Parser (Uuid -> a) a
uuid =
    custom "UUID" Uuid.fromString
