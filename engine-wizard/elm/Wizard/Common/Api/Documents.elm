module Wizard.Common.Api.Documents exposing
    ( deleteDocument
    , downloadDocumentUrl
    , getDocuments
    , postDocument
    )

import Json.Decode as D
import Json.Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.Document as Document exposing (Document)


getDocuments : Maybe String -> AppState -> ToMsg (List Document) msg -> Cmd msg
getDocuments questionnaireUuid =
    let
        url =
            questionnaireUuid
                |> Maybe.map (\uuid -> "?questionnaireUuid=" ++ uuid)
                |> Maybe.withDefault ""
                |> (++) "/documents"
    in
    jwtGet url (D.list Document.decoder)


postDocument : Value -> AppState -> ToMsg Document msg -> Cmd msg
postDocument =
    jwtFetch "/documents" Document.decoder


deleteDocument : String -> AppState -> ToMsg () msg -> Cmd msg
deleteDocument uuid =
    jwtDelete ("/documents/" ++ uuid)


downloadDocumentUrl : String -> AppState -> String
downloadDocumentUrl uuid appState =
    appState.apiUrl ++ "/documents/" ++ uuid ++ "/download"
