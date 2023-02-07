module Url.Parser.Query.Extra exposing (bool, uuid)

import Dict
import Url.Parser.Query exposing (Parser, custom, enum)
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


bool : String -> Parser (Maybe Bool)
bool key =
    enum key (Dict.fromList [ ( "true", True ), ( "false", False ) ])
