module Registry.Pages.DocumentTemplates exposing
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
import Registry.Api.DocumentTemplates as DocumentTemplatesApi
import Registry.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Registry.Components.ListItem as ListItem
import Registry.Components.Page as Page
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Shared.Data.ApiError as ApiError exposing (ApiError)
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
                |> List.sortBy ((*) -1 << Time.posixToMillis << .createdAt)
                |> List.map (ListItem.view appState { toRoute = Routes.documentTemplateDetail << .id })
                |> div []
    in
    div [ class "my-5" ]
        [ h1 [ class "text-center mb-5" ] [ text (gettext "Document Templates" appState.locale) ]
        , documentTemplateView
        ]
