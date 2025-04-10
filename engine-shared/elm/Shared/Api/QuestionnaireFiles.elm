module Shared.Api.QuestionnaireFiles exposing (deleteFile, fileUrl, getFileUrl, getQuestionnaireFiles, postFile)

import File exposing (File)
import Http
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtOrHttpDelete, jwtOrHttpFetchFileWithData, jwtOrHttpGet)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireFile as QuestionnaireFile exposing (QuestionnaireFile)
import Shared.Data.QuestionnaireFileSimple as QuestionnaireFileSimple exposing (QuestionnaireFileSimple)
import Shared.Data.UrlResponse as UrlResponse exposing (UrlResponse)
import Uuid exposing (Uuid)


getQuestionnaireFiles : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireFile) msg -> Cmd msg
getQuestionnaireFiles _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-files" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaireFiles" QuestionnaireFile.decoder)


postFile : Uuid -> String -> File -> AbstractAppState a -> ToMsg QuestionnaireFileSimple msg -> Cmd msg
postFile questionnaireUuid questionUuidString file =
    jwtOrHttpFetchFileWithData ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ questionUuidString)
        [ Http.stringPart "fileName" (File.name file) ]
        QuestionnaireFileSimple.decoder
        file


deleteFile : Uuid -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteFile questionnaireUuid fileUuid =
    jwtOrHttpDelete ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ Uuid.toString fileUuid)


getFileUrl : Uuid -> Uuid -> AbstractAppState a -> ToMsg UrlResponse msg -> Cmd msg
getFileUrl projectUuid fileUuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString projectUuid ++ "/files/" ++ Uuid.toString fileUuid) UrlResponse.decoder


fileUrl : Uuid -> Uuid -> AbstractAppState a -> String
fileUrl questionnaireUuid fileUuid appState =
    appState.apiUrl ++ "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files/" ++ Uuid.toString fileUuid
