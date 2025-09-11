module Common.Api.Models.DevOperation.DevOperationParameterType exposing
    ( DevOperationParameterType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type DevOperationParameterType
    = String
    | Int
    | Double
    | Bool
    | Json
    | Tenant


fromString : String -> Maybe DevOperationParameterType
fromString str =
    case str of
        "StringDevOperationParameterType" ->
            Just String

        "IntDevOperationParameterType" ->
            Just Int

        "DoubleDevOperationParameterType" ->
            Just Double

        "BoolDevOperationParameterType" ->
            Just Bool

        "JsonDevOperationParameterType" ->
            Just Json

        "TenantDevOperationParameterType" ->
            Just Tenant

        _ ->
            Nothing


decoder : Decoder DevOperationParameterType
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown dev operation parameter type: " ++ str
            )
