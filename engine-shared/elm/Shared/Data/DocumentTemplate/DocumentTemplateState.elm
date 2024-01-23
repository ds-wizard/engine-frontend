module Shared.Data.DocumentTemplate.DocumentTemplateState exposing
    ( DocumentTemplateState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type DocumentTemplateState
    = Default
    | UnsupportedMetamodelVersion


decoder : Decoder DocumentTemplateState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "DefaultDocumentTemplateState" ->
                        D.succeed Default

                    "UnsupportedMetamodelVersionDocumentTemplateState" ->
                        D.succeed UnsupportedMetamodelVersion

                    _ ->
                        D.fail <| "Unknown document template state: " ++ str
            )
