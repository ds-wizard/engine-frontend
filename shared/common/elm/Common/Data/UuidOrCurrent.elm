module Common.Data.UuidOrCurrent exposing
    ( UuidOrCurrent
    , current
    , empty
    , encode
    , isCurrent
    , matchUuid
    , parser
    , toString
    , uuid
    )

import Json.Encode as E
import Url.Parser as Parser exposing (Parser)
import Uuid exposing (Uuid)


type UuidOrCurrent
    = Uuid Uuid
    | Current


current : UuidOrCurrent
current =
    Current


uuid : Uuid -> UuidOrCurrent
uuid =
    Uuid


empty : UuidOrCurrent
empty =
    Uuid Uuid.nil


currentString : String
currentString =
    "current"


fromString : String -> Maybe UuidOrCurrent
fromString text =
    if text == currentString then
        Just Current

    else
        Maybe.map Uuid (Uuid.fromString text)


parser : Parser (UuidOrCurrent -> a) a
parser =
    Parser.custom "UuidOrCurrent" fromString


toString : UuidOrCurrent -> String
toString uuidOrCurrent =
    case uuidOrCurrent of
        Uuid u ->
            Uuid.toString u

        Current ->
            currentString


isCurrent : UuidOrCurrent -> Bool
isCurrent uuidOrCurrent =
    case uuidOrCurrent of
        Current ->
            True

        _ ->
            False


matchUuid : UuidOrCurrent -> Uuid -> Bool
matchUuid uuidOrCurrent testUuid =
    case uuidOrCurrent of
        Current ->
            False

        Uuid u ->
            u == testUuid


encode : UuidOrCurrent -> E.Value
encode =
    E.string << toString
