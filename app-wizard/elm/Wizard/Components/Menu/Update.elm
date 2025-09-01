module Wizard.Components.Menu.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Dict
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError
import Shared.Data.BuildInfo as BuildInfo
import Shared.Ports.Copy as Copy
import Task
import Wizard.Api.BuildInfo as BuildInfoApi
import Wizard.Components.Menu.Models exposing (Model)
import Wizard.Components.Menu.Msgs exposing (Msg(..))
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs


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

        CopyAbout ->
            case model.apiBuildInfo of
                Success apiBuildInfo ->
                    let
                        componentToString name component =
                            name ++ "\nVersion: " ++ component.version ++ "\nBuilt at: " ++ component.builtAt

                        parts =
                            componentToString (gettext "Client" appState.locale) BuildInfo.client
                                :: componentToString (gettext "Server" appState.locale) apiBuildInfo
                                :: List.map (\c -> componentToString c.name c) (List.sortBy .name apiBuildInfo.components)

                        aboutString =
                            String.join "\n---\n" parts
                    in
                    ( { model | recentlyCopied = True }, Copy.copyToClipboard aboutString )

                _ ->
                    ( model, Cmd.none )

        ClearRecentlyCopied ->
            ( { model | recentlyCopied = False }, Cmd.none )

        GetBuildInfoCompleted result ->
            case result of
                Ok buildInfo ->
                    ( { model | apiBuildInfo = Success buildInfo }, Cmd.none )

                Err error ->
                    ( { model | apiBuildInfo = ApiError.toActionResult appState (gettext "Unable to get the build info" appState.locale) error }, Cmd.none )

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
