module Shared.Data.Document.DocumentState exposing
    ( DocumentState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type DocumentState
    = QueuedDocumentState
    | InProgressDocumentState
    | DoneDocumentState
    | ErrorDocumentState


decoder : Decoder DocumentState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "QueuedDocumentState" ->
                        D.succeed QueuedDocumentState

                    "InProgressDocumentState" ->
                        D.succeed InProgressDocumentState

                    "DoneDocumentState" ->
                        D.succeed DoneDocumentState

                    "ErrorDocumentState" ->
                        D.succeed ErrorDocumentState

                    unknownState ->
                        D.fail <| "Unknown document state " ++ unknownState
            )
