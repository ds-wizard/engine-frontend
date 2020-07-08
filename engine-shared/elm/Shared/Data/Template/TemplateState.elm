module Shared.Data.Template.TemplateState exposing
    ( TemplateState
    , decoder
    , isOutdated
    , unknown
    )

import Json.Decode as D exposing (Decoder)


type TemplateState
    = UnknownTemplateState
    | OutdatedTemplateState
    | UpToDateTemplateState
    | UnpublishedTemplateState


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

                    _ ->
                        D.fail <| "Unknown template state: " ++ str
            )


isOutdated : TemplateState -> Bool
isOutdated =
    (==) OutdatedTemplateState
