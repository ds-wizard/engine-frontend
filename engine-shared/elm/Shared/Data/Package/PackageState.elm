module Shared.Data.Package.PackageState exposing
    ( PackageState
    , decoder
    , isOutdated
    , unknown
    )

import Json.Decode as D exposing (Decoder)


type PackageState
    = UnknownPackageState
    | OutdatedPackageState
    | UpToDatePackageState
    | UnpublishedPackageState


unknown : PackageState
unknown =
    UnknownPackageState


decoder : Decoder PackageState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UnknownPackageState" ->
                        D.succeed UnknownPackageState

                    "OutdatedPackageState" ->
                        D.succeed OutdatedPackageState

                    "UpToDatePackageState" ->
                        D.succeed UpToDatePackageState

                    "UnpublishedPackageState" ->
                        D.succeed UnpublishedPackageState

                    _ ->
                        D.fail <| "Unknown package state: " ++ str
            )


isOutdated : PackageState -> Bool
isOutdated =
    (==) OutdatedPackageState
