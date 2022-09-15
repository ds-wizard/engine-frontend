module Wizard.Common.Menu.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Dict
import Shared.Api.BuildInfo as BuildInfoApi
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (l)
import Task
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Menu.Models exposing (Model)
import Wizard.Common.Menu.Msgs exposing (Msg(..))
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Menu.Update"


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        SetReportIssueOpen open ->
            ( { model | reportIssueOpen = open }, Cmd.none )

        SetAboutOpen open ->
            let
                ( apiBuildInfo, cmd ) =
                    if open then
                        ( Loading, loadBuildInfo wrapMsg appState )

                    else
                        ( Unset, Cmd.none )
            in
            ( { model | aboutOpen = open, apiBuildInfo = apiBuildInfo }, cmd )

        GetBuildInfoCompleted result ->
            case result of
                Ok buildInfo ->
                    ( { model | apiBuildInfo = Success buildInfo }, Cmd.none )

                Err error ->
                    ( { model | apiBuildInfo = ApiError.toActionResult appState (l_ "error.buildInfo" appState) error }, Cmd.none )

        DevMenuDropdownMsg dropdownState ->
            ( { model | devMenuDropdownState = dropdownState }, Cmd.none )

        HelpMenuDropdownMsg dropdownState ->
            ( { model | helpMenuDropdownState = dropdownState }, Cmd.none )

        ProfileMenuDropdownMsg dropdownState ->
            ( { model | profileMenuDropdownState = dropdownState }, Cmd.none )

        GetElement elementId ->
            ( model, Task.attempt (wrapMsg << GotElement elementId) (Dom.getElement elementId) )

        GotElement elementId result ->
            case result of
                Ok element ->
                    ( { model | submenuPositions = Dict.insert elementId element Dict.empty }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        HideElement elementId ->
            ( { model | submenuPositions = Dict.remove elementId model.submenuPositions }, Cmd.none )


loadBuildInfo : (Msg -> Wizard.Msgs.Msg) -> AppState -> Cmd Wizard.Msgs.Msg
loadBuildInfo wrapMsg appState =
    Cmd.map wrapMsg <|
        BuildInfoApi.getBuildInfo appState GetBuildInfoCompleted
