module Shared.Data.Package.PackagePhase exposing
    ( PackagePhase(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type PackagePhase
    = Released
    | Deprecated


decoder : Decoder PackagePhase
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "ReleasedPackagePhase" ->
                        D.succeed Released

                    "DeprecatedPackagePhase" ->
                        D.succeed Deprecated

                    _ ->
                        D.fail <| "Unknown document template phase: " ++ str
            )


encode : PackagePhase -> E.Value
encode =
    E.string << toString


toString : PackagePhase -> String
toString phase =
    case phase of
        Released ->
            "ReleasedPackagePhase"

        Deprecated ->
            "DeprecatedPackagePhase"
