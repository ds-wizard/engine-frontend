module Common.Pages.KnowledgeModels.ResourcePage exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| One resource page of a knowledge model, as a page of its own.

The page's title is its `h1` and the collection it belongs to is the
description under it. The content's own headings are demoted beneath the title.

-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Components.Markdown as Markdown
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModel)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Page.Content as Content
import Uuid exposing (Uuid)


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , resourcePageUuid : String
    }


initialModel : String -> Model
initialModel resourcePageUuid =
    { knowledgeModel = ActionResult.Loading
    , resourcePageUuid = resourcePageUuid
    }


init : AppState -> Uuid -> String -> ( Model, Cmd Msg )
init appState kmUuid resourcePageUuid =
    ( initialModel resourcePageUuid
    , KnowledgeModelsApi.fetchPreview appState (Just kmUuid) [] [] FetchPreviewComplete
    )


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FetchPreviewComplete result ->
            RequestHelpers.applyResultOrNotFound
                { setResult = setKnowledgeModel
                , defaultError = gettext "Unable to get resource page." appState.locale
                , notFoundCmd = UpdateConfig.notFound cfg (NotFound.knowledgeModel appState)
                , notAllowedCmd = UpdateConfig.notAllowed cfg
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewResourcePage appState model) model.knowledgeModel


viewResourcePage : AppState -> Model -> KnowledgeModel -> Html Msg
viewResourcePage appState model km =
    case
        ( KnowledgeModel.getResourcePage model.resourcePageUuid km
        , KnowledgeModel.getResourceCollectionByResourcePageUuid model.resourcePageUuid km
        )
    of
        ( Just resourcePage, Just resourceCollection ) ->
            Content.content
                { title = resourcePage.title
                , body = [ Markdown.view { headingOffset = 1 } resourcePage.content ]
                }
                |> Content.withDescription resourceCollection.title
                |> Content.narrow
                |> Content.toHtml

        _ ->
            -- The knowledge model is here and this page of it is not: the same
            -- answer as a missing knowledge model, since to the reader the
            -- address named something that is not there either way.
            let
                notFound =
                    NotFound.resourcePage appState
            in
            Page.notFoundEntity appState
                { message = notFound.message
                , back = NotFound.back appState notFound
                }
