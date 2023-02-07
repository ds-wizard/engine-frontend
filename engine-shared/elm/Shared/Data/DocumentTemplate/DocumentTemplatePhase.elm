module Shared.Data.DocumentTemplate.DocumentTemplatePhase exposing
    ( DocumentTemplatePhase(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type DocumentTemplatePhase
    = Draft
    | Released
    | Deprecated


decoder : Decoder DocumentTemplatePhase
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "DraftDocumentTemplatePhase" ->
                        D.succeed Draft

                    "ReleasedDocumentTemplatePhase" ->
                        D.succeed Released

                    "DeprecatedDocumentTemplatePhase" ->
                        D.succeed Deprecated

                    _ ->
                        D.fail <| "Unknown document template phase: " ++ str
            )


encode : DocumentTemplatePhase -> E.Value
encode =
    E.string << toString


toString : DocumentTemplatePhase -> String
toString phase =
    case phase of
        Draft ->
            "DraftDocumentTemplatePhase"

        Released ->
            "ReleasedDocumentTemplatePhase"

        Deprecated ->
            "DeprecatedDocumentTemplatePhase"
