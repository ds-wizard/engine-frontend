module Shared.Api.QuestionnaireImporters exposing
    ( getQuestionnaireImporter
    , getQuestionnaireImporters
    , getQuestionnaireImportersFor
    , putQuestionnaireImporter
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtPut)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireImporter as QuestionnaireImporter exposing (QuestionnaireImporter)
import Uuid exposing (Uuid)


getQuestionnaireImporters : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireImporter) msg -> Cmd msg
getQuestionnaireImporters qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-importers" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaireImporters" QuestionnaireImporter.decoder)


getQuestionnaireImporter : String -> AbstractAppState a -> ToMsg QuestionnaireImporter msg -> Cmd msg
getQuestionnaireImporter questionnaireImporterId =
    jwtGet ("/questionnaire-importers/" ++ questionnaireImporterId) QuestionnaireImporter.decoder


putQuestionnaireImporter : QuestionnaireImporter -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireImporter questionnaireImporter =
    let
        body =
            QuestionnaireImporter.encode questionnaireImporter
    in
    jwtPut ("/questionnaire-importers/" ++ questionnaireImporter.id) body


getQuestionnaireImportersFor : Uuid -> AbstractAppState a -> ToMsg (List QuestionnaireImporter) msg -> Cmd msg
getQuestionnaireImportersFor questionnaireUuid =
    let
        paginationQueryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "name") PaginationQueryString.SortASC

        queryString =
            PaginationQueryString.toApiUrlWith [ ( "questionnaireUuid", Uuid.toString questionnaireUuid ), ( "enabled", "true" ) ] paginationQueryString

        url =
            "/questionnaire-importers/suggestions" ++ queryString
    in
    jwtGet url (D.map .items (Pagination.decoder "questionnaireImporters" QuestionnaireImporter.decoder))
