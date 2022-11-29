module Registry.Pages.Locales exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h5, p, small, text)
import Html.Attributes exposing (class, href)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.Locale exposing (Locale)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Error.ApiError as ApiError exposing (ApiError)


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { locales = Loading }
    , Requests.getLocales appState GetLocalesComplete
    )



-- MODEL


type alias Model =
    { locales : ActionResult (List Locale) }


setPackages : ActionResult (List Locale) -> Model -> Model
setPackages locales model =
    { model | locales = locales }



-- UPDATE


type Msg
    = GetLocalesComplete (Result ApiError (List Locale))


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        GetLocalesComplete result ->
            ActionResult.apply setPackages (ApiError.toActionResult appState (gettext "Unable to get locales." appState.locale)) result



-- VIEW


view : Model -> Html Msg
view model =
    Page.actionResultView viewList model.locales


viewList : List Locale -> Html Msg
viewList locale =
    div []
        [ div [ class "list-group list-group-flush" ]
            (List.map viewItem <| List.sortBy .name locale)
        ]


viewItem : Locale -> Html Msg
viewItem locale =
    let
        packageLink =
            Routing.toString <| Routing.LocaleDetail locale.id
    in
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href packageLink ] [ text locale.name ]
                ]
            , small [] [ text locale.organization.name ]
            ]
        , p [] [ text locale.description ]
        ]
