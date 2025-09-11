module Wizard.Pages.Dev.PersistentCommandsDetail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState
import Common.Utils.RequestHelpers as RequestHelpers
import Uuid exposing (Uuid)
import Wizard.Api.PersistentCommands as PersistentCommandsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    PersistentCommandsApi.getPersistentCommand appState uuid GerPersistentCommandComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GerPersistentCommandComplete result ->
            RequestHelpers.applyResult
                { setResult = \res m -> { m | persistentCommand = res }
                , defaultError = "Unable to get persistent command."
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        DropdownMsg state ->
            ( { model | dropdownState = state }, Cmd.none )

        RerunCommand ->
            ( { model | updating = Loading }
            , PersistentCommandsApi.retry appState model.uuid (wrapMsg << RerunCommandComplete)
            )

        RerunCommandComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = \res m -> { m | updating = res }
                , defaultError = "Unable to rerun persistent command."
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always "Persistent command has been scheduled for the rerun."
                , locale = appState.locale
                }

        SetIgnored ->
            ( { model | updating = Loading }
            , PersistentCommandsApi.updateState appState model.uuid PersistentCommandState.Ignore (wrapMsg << SetIgnoredComplete)
            )

        SetIgnoredComplete result ->
            case model.persistentCommand of
                Success persistentCommand ->
                    case result of
                        Ok _ ->
                            ( { model
                                | persistentCommand = Success { persistentCommand | state = PersistentCommandState.Ignore }
                                , updating = Success "Persistent command is now being ignored."
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model | updating = ApiError.toActionResult appState "Unable to set persistent command ignored." error }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )
