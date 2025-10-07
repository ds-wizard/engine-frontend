module Wizard.Pages.KnowledgeModels.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setPulling)
import Gettext exposing (gettext)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangePackageId packageId ->
            ( { model | packageId = packageId }, Cmd.none )

        Submit ->
            if String.length model.packageId > 0 then
                ( { model | pulling = Loading }
                , PackagesApi.pullPackage appState model.packageId (wrapMsg << PullPackageCompleted)
                )

            else
                ( model, Cmd.none )

        PullPackageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setPulling
                , defaultError = gettext "Unable to import the package." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
