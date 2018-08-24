module DSPlanner.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(Success))
import Bootstrap.Dropdown as Dropdown
import DSPlanner.Index.Models exposing (Model, QuestionnaireRow)
import DSPlanner.Index.Msgs exposing (Msg(DropdownMsg))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    case model.questionnaires of
        Success rows ->
            Sub.batch <| List.map (rowSubscriptions wrapMsg) rows

        _ ->
            Sub.none


rowSubscriptions : (Msg -> Msgs.Msg) -> QuestionnaireRow -> Sub Msgs.Msg
rowSubscriptions wrapMsg row =
    Dropdown.subscriptions row.dropdownState (wrapMsg << DropdownMsg row.questionnaire)
