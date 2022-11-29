module Wizard.Templates.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Templates as TemplatesApi
import Shared.Setters exposing (setPulling)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Templates.Import.RegistryImport.Models exposing (Model)
import Wizard.Templates.Import.RegistryImport.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeTemplateId packageId ->
            ( { model | templateId = packageId }, Cmd.none )

        Submit ->
            if String.length model.templateId > 0 then
                ( { model | pulling = Loading }
                , TemplatesApi.pullTemplate model.templateId appState (wrapMsg << PullTemplateCompleted)
                )

            else
                ( model, Cmd.none )

        PullTemplateCompleted result ->
            applyResult appState
                { setResult = setPulling
                , defaultError = gettext "Unable to import the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }
