module Shared.Data.Template.TemplateState exposing
    ( TemplateState
    , decoder
    , isOutdated
    , isUnsupported
    , unknown
    )

import Json.Decode as D exposing (Decoder)


type TemplateState
    = UnknownTemplateState
    | OutdatedTemplateState
    | UpToDateTemplateState
    | UnpublishedTemplateState
    | UnsupportedMetamodelVersion


unknown : TemplateState
unknown =
    UnknownTemplateState


decoder : Decoder TemplateState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UnknownTemplateState" ->
                        D.succeed UnknownTemplateState

                    "OutdatedTemplateState" ->
                        D.succeed OutdatedTemplateState

                    "UpToDateTemplateState" ->
                        D.succeed UpToDateTemplateState

                    "UnpublishedTemplateState" ->
                        D.succeed UnpublishedTemplateState

                    "UnsupportedMetamodelVersion" ->
                        D.succeed UnsupportedMetamodelVersion

                    _ ->
                        D.fail <| "Unknown template state: " ++ str
            )


isOutdated : TemplateState -> Bool
isOutdated =
    (==) OutdatedTemplateState


isUnsupported : TemplateState -> Bool
isUnsupported =
    (==) UnsupportedMetamodelVersion
