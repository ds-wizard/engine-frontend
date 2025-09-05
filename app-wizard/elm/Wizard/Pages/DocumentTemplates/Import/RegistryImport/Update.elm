module Wizard.Pages.DocumentTemplates.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setPulling)
import Gettext exposing (gettext)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeTemplateId packageId ->
            ( { model | documentTemplateId = packageId }, Cmd.none )

        Submit ->
            if String.length model.documentTemplateId > 0 then
                ( { model | pulling = Loading }
                , DocumentTemplatesApi.pullTemplate appState model.documentTemplateId (wrapMsg << PullTemplateCompleted)
                )

            else
                ( model, Cmd.none )

        PullTemplateCompleted result ->
            RequestHelpers.applyResult
                { setResult = setPulling
                , defaultError = gettext "Unable to import the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
