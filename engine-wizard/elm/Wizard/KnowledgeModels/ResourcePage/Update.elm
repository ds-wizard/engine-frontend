module Wizard.KnowledgeModels.ResourcePage.Update exposing
    ( fetchData
    , update
    )

import Gettext exposing (gettext)
import Shared.Api.KnowledgeModels as KnowlegeModelsApi
import Shared.Setters exposing (setKnowledgeModel)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.ResourcePage.Models exposing (Model)
import Wizard.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> String -> Cmd Msg
fetchData appState kmId =
    KnowlegeModelsApi.fetchPreview (Just kmId) [] [] appState FetchPreviewComplete


update : AppState -> Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg model =
    case msg of
        FetchPreviewComplete result ->
            applyResult appState
                { setResult = setKnowledgeModel
                , defaultError = gettext "Unable to get resource page." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }
