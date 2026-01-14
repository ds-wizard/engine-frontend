module Wizard.Pages.Locales.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setLocale)
import Gettext exposing (gettext)
import Wizard.Api.Locales as LocalesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Locales.Import.RegistryImport.Models exposing (Model)
import Wizard.Pages.Locales.Import.RegistryImport.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeLocaleId localeId ->
            ( { model | localeId = localeId }, Cmd.none )

        Submit ->
            if String.length model.localeId > 0 then
                ( { model | locale = Loading }
                , LocalesApi.pullLocale appState model.localeId (wrapMsg << PullLocaleCompleted)
                )

            else
                ( model, Cmd.none )

        PullLocaleCompleted result ->
            RequestHelpers.applyResult
                { setResult = setLocale
                , defaultError = gettext "Unable to import the locale." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
