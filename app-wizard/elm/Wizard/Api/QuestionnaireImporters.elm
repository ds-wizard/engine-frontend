module Wizard.Api.QuestionnaireImporters exposing
    ( getQuestionnaireImporter
    , getQuestionnaireImporters
    , getQuestionnaireImportersFor
    , putQuestionnaireImporter
    )

import Json.Decode as D
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireImporter as QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Common.AppState as AppState exposing (AppState)


getQuestionnaireImporters : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireImporter) msg -> Cmd msg
getQuestionnaireImporters appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-importers" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaireImporters" QuestionnaireImporter.decoder)


getQuestionnaireImporter : AppState -> String -> ToMsg QuestionnaireImporter msg -> Cmd msg
getQuestionnaireImporter appState questionnaireImporterId =
    Request.get (AppState.toServerInfo appState) ("/questionnaire-importers/" ++ questionnaireImporterId) QuestionnaireImporter.decoder


putQuestionnaireImporter : AppState -> QuestionnaireImporter -> ToMsg () msg -> Cmd msg
putQuestionnaireImporter appState questionnaireImporter =
    let
        body =
            QuestionnaireImporter.encode questionnaireImporter
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaire-importers/" ++ questionnaireImporter.id) body


getQuestionnaireImportersFor : AppState -> Uuid -> ToMsg (List QuestionnaireImporter) msg -> Cmd msg
getQuestionnaireImportersFor appState questionnaireUuid =
    let
        paginationQueryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "name") PaginationQueryString.SortASC

        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "questionnaireUuid", Uuid.toString questionnaireUuid )
                , ( "enabled", "true" )
                ]
                paginationQueryString

        url =
            "/questionnaire-importers/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (D.map .items (Pagination.decoder "questionnaireImporters" QuestionnaireImporter.decoder))
