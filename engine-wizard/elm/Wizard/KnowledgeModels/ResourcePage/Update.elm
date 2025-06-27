module Wizard.KnowledgeModels.ResourcePage.Update exposing
    ( fetchData
    , update
    )

import Gettext exposing (gettext)
import Shared.Setters exposing (setKnowledgeModel)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.KnowledgeModels as KnowlegeModelsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.ResourcePage.Models exposing (Model)
import Wizard.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))
import Wizard.Msgs


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
