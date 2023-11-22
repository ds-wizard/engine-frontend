module Shared.Api.QuestionnaireActions exposing
    ( getQuestionnaireActions
    , getQuestionnaireActionsFor
    , putQuestionnaireAction
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtPut)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireAction as QuestionnaireAction exposing (QuestionnaireAction)
import Uuid exposing (Uuid)


getQuestionnaireActions : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireAction) msg -> Cmd msg
getQuestionnaireActions _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaire-actions" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaireActions" QuestionnaireAction.decoder)


putQuestionnaireAction : QuestionnaireAction -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireAction questionnaireImporter =
    let
        body =
            QuestionnaireAction.encode questionnaireImporter
    in
    jwtPut ("/questionnaire-actions/" ++ questionnaireImporter.id) body


getQuestionnaireActionsFor : Uuid -> AbstractAppState a -> ToMsg (List QuestionnaireAction) msg -> Cmd msg
getQuestionnaireActionsFor questionnaireUuid =
    let
        paginationQueryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "name") PaginationQueryString.SortASC

        queryString =
            PaginationQueryString.toApiUrlWith [ ( "questionnaireUuid", Uuid.toString questionnaireUuid ), ( "enabled", "true" ) ] paginationQueryString

        url =
            "/questionnaire-actions/suggestions" ++ queryString
    in
    jwtGet url (D.map .items (Pagination.decoder "questionnaireActions" QuestionnaireAction.decoder))
