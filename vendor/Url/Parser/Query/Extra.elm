module Url.Parser.Query.Extra exposing (uuid)

import Url.Parser.Query exposing (Parser, custom)
import Uuid exposing (Uuid)


uuid : String -> Parser (Maybe Uuid)
uuid key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Uuid.fromString str

                _ ->
                    Nothing
