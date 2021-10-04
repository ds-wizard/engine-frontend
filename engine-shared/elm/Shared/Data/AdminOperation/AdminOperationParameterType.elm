module Shared.Data.AdminOperation.AdminOperationParameterType exposing
    ( AdminOperationParameterType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type AdminOperationParameterType
    = String
    | Int
    | Double
    | Bool
    | Json


fromString : String -> Maybe AdminOperationParameterType
fromString str =
    case str of
        "StringAdminOperationParameterType" ->
            Just String

        "IntAdminOperationParameterType" ->
            Just Int

        "DoubleAdminOperationParameterType" ->
            Just Double

        "BoolAdminOperationParameterType" ->
            Just Bool

        "JsonAdminOperationParameterType" ->
            Just Json

        _ ->
            Nothing


decoder : Decoder AdminOperationParameterType
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown admin operation parameter type: " ++ str
            )
