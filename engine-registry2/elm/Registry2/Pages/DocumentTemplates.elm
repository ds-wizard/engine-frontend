module Registry2.Pages.DocumentTemplates exposing
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
import Registry2.Api.DocumentTemplates as DocumentTemplatesApi
import Registry2.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Registry2.Components.ListItem as ListItem
import Registry2.Components.Page as Page
import Registry2.Data.AppState exposing (AppState)
import Registry2.Routes as Routes
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Time


type alias Model =
    { documentTemplates : ActionResult (List DocumentTemplate) }


initialModel : Model
initialModel =
    { documentTemplates = ActionResult.Loading }


setDocumentTemplates : ActionResult (List DocumentTemplate) -> Model -> Model
setDocumentTemplates result model =
    { model | documentTemplates = result }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , DocumentTemplatesApi.getDocumentTemplates appState GetDocumentTemplatesCompleted
    )


type Msg
    = GetDocumentTemplatesCompleted (Result ApiError (List DocumentTemplate))


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetDocumentTemplatesCompleted result ->
            ( ActionResult.apply setDocumentTemplates
                (ApiError.toActionResult appState (gettext "Unable to get document templates." appState.locale))
                result
                model
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewDocumentTemplates appState) model.documentTemplates


viewDocumentTemplates : AppState -> List DocumentTemplate -> Html Msg
viewDocumentTemplates appState documentTemplates =
    let
        documentTemplateView =
            documentTemplates
                |> List.sortBy (Time.toMillis appState.timeZone << .createdAt)
                |> List.map (ListItem.view appState { toRoute = Routes.documentTemplateDetail << .id })
                |> div []
    in
    div []
        [ h1 [ class "text-center mb-5" ] [ text (gettext "Document Templates" appState.locale) ]
        , documentTemplateView
        ]
