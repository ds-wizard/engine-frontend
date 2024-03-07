module Registry.Pages.Locales exposing
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
import Registry.Api.Locales as LocalesApi
import Registry.Api.Models.Locale exposing (Locale)
import Registry.Components.ListItem as ListItem
import Registry.Components.Page as Page
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Time


type alias Model =
    { locales : ActionResult (List Locale) }


initialModel : Model
initialModel =
    { locales = ActionResult.Loading }


setLocales : ActionResult (List Locale) -> Model -> Model
setLocales result model =
    { model | locales = result }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , LocalesApi.getLocales appState GetLocalesCompleted
    )


type Msg
    = GetLocalesCompleted (Result ApiError (List Locale))


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetLocalesCompleted result ->
            ( ActionResult.apply setLocales
                (ApiError.toActionResult appState (gettext "Unable to get locales." appState.locale))
                result
                model
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewLocales appState) model.locales


viewLocales : AppState -> List Locale -> Html Msg
viewLocales appState documentTemplates =
    let
        localeView =
            documentTemplates
                |> List.sortBy (Time.toMillis appState.timeZone << .createdAt)
                |> List.map (ListItem.view appState { toRoute = Routes.localeDetail << .id })
                |> div []
    in
    div [ class "my-5" ]
        [ h1 [ class "text-center mb-5" ] [ text (gettext "Locales" appState.locale) ]
        , localeView
        ]
