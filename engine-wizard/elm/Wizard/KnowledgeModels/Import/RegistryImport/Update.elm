module Wizard.KnowledgeModels.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (l, lg)
import Wizard.Common.Setters exposing (setPulling)
import Wizard.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Msgs


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangePackageId packageId ->
            ( { model | packageId = packageId }, Cmd.none )

        Submit ->
            if String.length model.packageId > 0 then
                ( { model | pulling = Loading }
                , PackagesApi.pullPackage model.packageId appState (wrapMsg << PullPackageCompleted)
                )

            else
                ( model, Cmd.none )

        PullPackageCompleted result ->
            applyResult
                { setResult = setPulling
                , defaultError = lg "apiError.packages.pullError" appState
                , model = model
                , result = result
                }
