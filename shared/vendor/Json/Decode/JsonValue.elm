-- TODO: Remove this module and use json-tools/json-value instead


module Json.Decode.JsonValue exposing
    ( JsonValue(..)
    , decoder
    , encode
    , fromString
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe
import String.Extra as String


type JsonValue
    = IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | StringValue String
    | NullValue


decoder : Decoder JsonValue
decoder =
    D.oneOf
        [ D.map IntValue D.int
        , D.map FloatValue D.float
        , D.map BoolValue D.bool
        , D.map StringValue D.string
        , D.null NullValue
        ]


encode : JsonValue -> E.Value
encode value =
    case value of
        IntValue int ->
            E.int int

        FloatValue float ->
            E.float float

        BoolValue bool ->
            E.bool bool

        StringValue string ->
            E.string string

        NullValue ->
            E.null


validation : Validation e JsonValue
validation v =
    Field.asString v
        |> Maybe.map fromString
        |> Result.fromMaybe (Error.value InvalidString)


fromString : String -> JsonValue
fromString value =
    let
        trimmedValue =
            String.trim value
    in
    if String.isEmpty trimmedValue then
        NullValue

    else
        Maybe.map IntValue (String.toInt trimmedValue)
            |> Maybe.orElse (Maybe.map FloatValue (String.toFloat trimmedValue))
            |> Maybe.orElse (Maybe.map BoolValue (String.toBool trimmedValue))
            |> Maybe.withDefault (StringValue trimmedValue)


toString : JsonValue -> String
toString value =
    case value of
        BoolValue bool ->
            String.fromBool bool

        FloatValue float ->
            String.fromFloat float

        IntValue int ->
            String.fromInt int

        StringValue string ->
            string

        NullValue ->
            ""
