module Wizard.Locales.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Locales as LocalesApi
import Shared.Setters exposing (setPulling)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Import.RegistryImport.Models exposing (Model)
import Wizard.Locales.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Msgs


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeLocaleId packageId ->
            ( { model | localeId = packageId }, Cmd.none )

        Submit ->
            if String.length model.localeId > 0 then
                ( { model | pulling = Loading }
                , LocalesApi.pullLocale model.localeId appState (wrapMsg << PullLocaleCompleted)
                )

            else
                ( model, Cmd.none )

        PullLocaleCompleted result ->
            applyResult appState
                { setResult = setPulling
                , defaultError = gettext "Unable to import the locale." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }
