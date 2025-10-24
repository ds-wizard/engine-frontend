module Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing
    ( KnowledgeModelPackagePhase(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type KnowledgeModelPackagePhase
    = Released
    | Deprecated


decoder : Decoder KnowledgeModelPackagePhase
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "ReleasedKnowledgeModelPackagePhase" ->
                        D.succeed Released

                    "DeprecatedKnowledgeModelPackagePhase" ->
                        D.succeed Deprecated

                    _ ->
                        D.fail <| "Unknown knowledge model package phase: " ++ str
            )


encode : KnowledgeModelPackagePhase -> E.Value
encode =
    E.string << toString


toString : KnowledgeModelPackagePhase -> String
toString phase =
    case phase of
        Released ->
            "ReleasedKnowledgeModelPackagePhase"

        Deprecated ->
            "DeprecatedKnowledgeModelPackagePhase"
