module Wizard.Api.Models.Document.DocumentState exposing
    ( DocumentState(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


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


encode : DocumentState -> E.Value
encode documentState =
    case documentState of
        QueuedDocumentState ->
            E.string "QueuedDocumentState"

        InProgressDocumentState ->
            E.string "InProgressDocumentState"

        DoneDocumentState ->
            E.string "DoneDocumentState"

        ErrorDocumentState ->
            E.string "ErrorDocumentState"
