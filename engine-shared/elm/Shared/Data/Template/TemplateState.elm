module Shared.Data.Template.TemplateState exposing
    ( TemplateState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type TemplateState
    = Unknown
    | Outdated
    | UpToDate
    | Unpublished
    | UnsupportedMetamodelVersion


decoder : Decoder TemplateState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UnknownTemplateState" ->
                        D.succeed Unknown

                    "OutdatedTemplateState" ->
                        D.succeed Outdated

                    "UpToDateTemplateState" ->
                        D.succeed UpToDate

                    "UnpublishedTemplateState" ->
                        D.succeed Unpublished

                    "UnsupportedMetamodelVersionTemplateState" ->
                        D.succeed UnsupportedMetamodelVersion

                    _ ->
                        D.fail <| "Unknown template state: " ++ str
            )
