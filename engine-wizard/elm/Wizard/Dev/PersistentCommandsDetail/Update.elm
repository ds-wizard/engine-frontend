module Wizard.Dev.PersistentCommandsDetail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.PersistentCommands as PersistentCommandsApi
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, applyResultTransform)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    PersistentCommandsApi.getPersistentCommand uuid appState GerPersistentCommandComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GerPersistentCommandComplete result ->
            applyResult appState
                { setResult = \res m -> { m | persistentCommand = res }
                , defaultError = "Unable to get persistent command."
                , model = model
                , result = result
                }

        RerunCommand ->
            ( { model | rerunning = Loading }
            , PersistentCommandsApi.retry model.uuid appState (wrapMsg << RerunCommandComplete)
            )

        RerunCommandComplete result ->
            applyResultTransform appState
                { setResult = \res m -> { m | rerunning = res }
                , defaultError = "Unable to rerun persistent command."
                , model = model
                , result = result
                , transform = always "Persistent command has been scheduled for the rerun."
                }
