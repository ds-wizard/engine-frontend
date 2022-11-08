module Shared.Data.DocumentTemplate.DocumentTemplateState exposing
    ( DocumentTemplateState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type DocumentTemplateState
    = Unknown
    | Outdated
    | UpToDate
    | Unpublished
    | UnsupportedMetamodelVersion


decoder : Decoder DocumentTemplateState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UnknownDocumentTemplateState" ->
                        D.succeed Unknown

                    "OutdatedDocumentTemplateState" ->
                        D.succeed Outdated

                    "UpToDateDocumentTemplateState" ->
                        D.succeed UpToDate

                    "UnpublishedDocumentTemplateState" ->
                        D.succeed Unpublished

                    "UnsupportedMetamodelVersionDocumentTemplateState" ->
                        D.succeed UnsupportedMetamodelVersion

                    _ ->
                        D.fail <| "Unknown document template state: " ++ str
            )
