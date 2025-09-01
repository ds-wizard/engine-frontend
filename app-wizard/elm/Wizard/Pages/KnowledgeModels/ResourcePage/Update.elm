module Wizard.Pages.KnowledgeModels.ResourcePage.Update exposing
    ( fetchData
    , update
    )

import Gettext exposing (gettext)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setKnowledgeModel)
import Wizard.Api.KnowledgeModels as KnowlegeModelsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.ResourcePage.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))


fetchData : AppState -> String -> Cmd Msg
fetchData appState kmId =
    KnowlegeModelsApi.fetchPreview appState (Just kmId) [] [] FetchPreviewComplete


update : AppState -> Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg model =
    case msg of
        FetchPreviewComplete result ->
            RequestHelpers.applyResult
                { setResult = setKnowledgeModel
                , defaultError = gettext "Unable to get resource page." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
