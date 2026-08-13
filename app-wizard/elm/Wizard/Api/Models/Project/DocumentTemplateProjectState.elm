module Wizard.Api.Models.Project.DocumentTemplateProjectState exposing
    ( DocumentTemplateProjectState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type DocumentTemplateProjectState
    = UpToDate
    | Outdated


decoder : Decoder DocumentTemplateProjectState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UpToDateDocumentTemplateProjectState" ->
                        D.succeed UpToDate

                    "OutdatedDocumentTemplateProjectState" ->
                        D.succeed Outdated

                    unknownState ->
                        D.fail <| "Unknown document template project state " ++ unknownState
            )
