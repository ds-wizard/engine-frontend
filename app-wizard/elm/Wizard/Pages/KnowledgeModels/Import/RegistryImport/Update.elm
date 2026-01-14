module Wizard.Pages.KnowledgeModels.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setPulling)
import Gettext exposing (gettext)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangePackageId kmPackageId ->
            ( { model | knwoledgeModelPackageId = kmPackageId }, Cmd.none )

        Submit ->
            if String.isEmpty model.knwoledgeModelPackageId then
                ( model, Cmd.none )

            else
                ( { model | pulling = Loading }
                , KnowledgeModelPackagesApi.pullKnowledgeModelPackage appState model.knwoledgeModelPackageId (wrapMsg << PullPackageCompleted)
                )

        PullPackageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setPulling
                , defaultError = gettext "Unable to import knowledge model." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
