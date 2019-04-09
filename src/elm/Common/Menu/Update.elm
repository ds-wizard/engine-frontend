module Common.Menu.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.BuildInfo as BuildInfoApi
import Common.ApiError exposing (getServerError)
import Common.AppState exposing (AppState)
import Common.Menu.Models exposing (Model)
import Common.Menu.Msgs exposing (Msg(..))
import Msgs


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        BuildInfoApi.getBuildInfo appState GetBuildInfoCompleted


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        SetReportIssueOpen open ->
            ( { model | reportIssueOpen = open }, Cmd.none )

        SetAboutOpen open ->
            let
                ( apiBuildInfo, cmd ) =
                    if open then
                        ( Loading, fetchData wrapMsg appState )

                    else
                        ( Unset, Cmd.none )
            in
            ( { model | aboutOpen = open, apiBuildInfo = apiBuildInfo }, cmd )

        GetBuildInfoCompleted result ->
            case result of
                Ok buildInfo ->
                    ( { model | apiBuildInfo = Success buildInfo }, Cmd.none )

                Err error ->
                    ( { model | apiBuildInfo = getServerError error "Cannot get build info" }, Cmd.none )

        HelpMenuDropdownMsg dropdownState ->
            ( { model | helpMenuDropdownState = dropdownState }, Cmd.none )

        ProfileMenuDropdownMsg dropdownState ->
            ( { model | profileMenuDropdownState = dropdownState }, Cmd.none )
