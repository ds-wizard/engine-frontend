module Wizard.Api.QuestionnaireActions exposing
    ( getQuestionnaireActions
    , getQuestionnaireActionsFor
    , putQuestionnaireAction
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireAction as QuestionnaireAction exposing (QuestionnaireAction)
import Wizard.Data.AppState as AppState exposing (AppState)


getQuestionnaireActions : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireAction) msg -> Cmd msg
getQuestionnaireActions appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-actions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaireActions" QuestionnaireAction.decoder)


putQuestionnaireAction : AppState -> QuestionnaireAction -> ToMsg () msg -> Cmd msg
putQuestionnaireAction appState questionnaireAction =
    let
        body =
            QuestionnaireAction.encode questionnaireAction
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaire-actions/" ++ questionnaireAction.id) body


getQuestionnaireActionsFor : AppState -> Uuid -> ToMsg (List QuestionnaireAction) msg -> Cmd msg
getQuestionnaireActionsFor appState questionnaireUuid =
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
            "/questionnaire-actions/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (D.map .items (Pagination.decoder "questionnaireActions" QuestionnaireAction.decoder))
