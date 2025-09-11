module Url.Parser.Query.Extensions exposing (bool, datetime, uuid)

import Dict
import Iso8601
import Time
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


datetime : String -> Parser (Maybe Time.Posix)
datetime key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Result.toMaybe (Iso8601.toTime str)

                _ ->
                    Nothing
