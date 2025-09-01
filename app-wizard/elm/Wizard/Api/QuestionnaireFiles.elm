module Wizard.Api.QuestionnaireFiles exposing
    ( deleteFile
    , fileUrl
    , getFileUrl
    , getQuestionnaireFiles
    , postFile
    )

import File exposing (File)
import Http
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireFile as QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Api.Models.QuestionnaireFileSimple as QuestionnaireFileSimple exposing (QuestionnaireFileSimple)
import Wizard.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Wizard.Common.AppState as AppState exposing (AppState)


getQuestionnaireFiles : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireFile) msg -> Cmd msg
getQuestionnaireFiles appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-files" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaireFiles" QuestionnaireFile.decoder)


postFile : AppState -> Uuid -> String -> File -> ToMsg QuestionnaireFileSimple msg -> Cmd msg
postFile appState questionnaireUuid questionUuidString file =
    let
        extraParts =
            [ Http.stringPart "fileName" (File.name file) ]

        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ questionUuidString
    in
    Request.postFileWithData (AppState.toServerInfo appState) url file extraParts QuestionnaireFileSimple.decoder


deleteFile : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteFile appState questionnaireUuid fileUuid =
    let
        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ Uuid.toString fileUuid
    in
    Request.delete (AppState.toServerInfo appState) url


getFileUrl : AppState -> Uuid -> Uuid -> ToMsg UrlResponse msg -> Cmd msg
getFileUrl appState projectUuid fileUuid =
    let
        url =
            "/questionnaires/" ++ Uuid.toString projectUuid ++ "/files/" ++ Uuid.toString fileUuid
    in
    Request.get (AppState.toServerInfo appState) url UrlResponse.decoder


fileUrl : AppState -> Uuid -> Uuid -> String
fileUrl appState questionnaireUuid fileUuid =
    appState.apiUrl ++ "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ Uuid.toString fileUuid
