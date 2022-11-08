module Wizard.DocumentTemplates.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Setters exposing (setPulling)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplates.Import.RegistryImport.Models exposing (Model)
import Wizard.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Msgs


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeTemplateId packageId ->
            ( { model | documentTemplateId = packageId }, Cmd.none )

        Submit ->
            if String.length model.documentTemplateId > 0 then
                ( { model | pulling = Loading }
                , DocumentTemplatesApi.pullTemplate model.documentTemplateId appState (wrapMsg << PullTemplateCompleted)
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
