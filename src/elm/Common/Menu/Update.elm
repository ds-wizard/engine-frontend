module Common.Menu.Update exposing (..)

import Common.Menu.Models exposing (Model)
import Common.Menu.Msgs exposing (Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetReportIssueOpen open ->
            { model | reportIssueOpen = open }

        ProfileMenuDropdownMsg state ->
            { model | profileMenuDropdownState = state }
