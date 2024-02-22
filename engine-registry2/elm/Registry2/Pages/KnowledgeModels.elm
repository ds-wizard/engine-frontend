module Registry2.Pages.KnowledgeModels exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Registry2.Api.KnowledgeModels as KnowledgeModelsApi
import Registry2.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Registry2.Components.ListItem as ListItem
import Registry2.Components.Page as Page
import Registry2.Data.AppState exposing (AppState)
import Registry2.Routes as Routes
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Time


type alias Model =
    { knowledgeModels : ActionResult (List KnowledgeModel) }


initialModel : Model
initialModel =
    { knowledgeModels = ActionResult.Loading }


setKnowledgeModels : ActionResult (List KnowledgeModel) -> Model -> Model
setKnowledgeModels result model =
    { model | knowledgeModels = result }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , KnowledgeModelsApi.getKnowledgeModels appState GetKnowledgeModelsCompleted
    )


type Msg
    = GetKnowledgeModelsCompleted (Result ApiError (List KnowledgeModel))


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            ( ActionResult.apply setKnowledgeModels
                (ApiError.toActionResult appState (gettext "Unable to get knowledge models." appState.locale))
                result
                model
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewKnowledgeModels appState) model.knowledgeModels


viewKnowledgeModels : AppState -> List KnowledgeModel -> Html Msg
viewKnowledgeModels appState knowledgeModels =
    let
        knowledgeModelsView =
            knowledgeModels
                |> List.sortBy (Time.toMillis appState.timeZone << .createdAt)
                |> List.map (ListItem.view appState { toRoute = Routes.knowledgeModelDetail << .id })
                |> div []
    in
    div [ class "my-5" ]
        [ h1 [ class "text-center mb-5" ] [ text (gettext "Knowledge Models" appState.locale) ]
        , knowledgeModelsView
        ]
