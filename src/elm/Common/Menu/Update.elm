module Common.Menu.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Menu.Models exposing (Model)
import Common.Menu.Msgs exposing (Msg(..))
import Common.Menu.Requests exposing (getBuildInfo)
import Common.Models exposing (getServerError)
import Http
import Msgs


fetchData : (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
fetchData wrapMsg =
    getBuildInfo
        |> Http.send GetBuildInfoCompleted
        |> Cmd.map wrapMsg


update : (Msg -> Msgs.Msg) -> Msg -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg model =
    case msg of
        SetReportIssueOpen open ->
            ( { model | reportIssueOpen = open }, Cmd.none )

        SetAboutOpen open ->
            let
                ( apiBuildInfo, cmd ) =
                    if open then
                        ( Loading, fetchData wrapMsg )
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

        ProfileMenuDropdownMsg state ->
            ( { model | profileMenuDropdownState = state }, Cmd.none )
