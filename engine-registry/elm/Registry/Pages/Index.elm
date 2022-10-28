module Registry.Pages.Index exposing
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
import Registry.Common.Entities.Package exposing (Package)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Error.ApiError as ApiError exposing (ApiError)


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { packages = Loading }
    , Requests.getPackages appState GetPackagesCompleted
    )



-- MODEL


type alias Model =
    { packages : ActionResult (List Package) }


setPackages : ActionResult (List Package) -> Model -> Model
setPackages packages model =
    { model | packages = packages }



-- UPDATE


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        GetPackagesCompleted result ->
            ActionResult.apply setPackages (ApiError.toActionResult appState (gettext "Unable to get the packages." appState.locale)) result



-- VIEW


view : Model -> Html Msg
view model =
    Page.actionResultView viewList model.packages


viewList : List Package -> Html Msg
viewList packages =
    div []
        [ div [ class "list-group list-group-flush" ]
            (List.map viewItem <| List.sortBy .name packages)
        ]


viewItem : Package -> Html Msg
viewItem package =
    let
        packageLink =
            Routing.toString <| Routing.KMDetail package.id
    in
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href packageLink ] [ text package.name ]
                ]
            , small [] [ text package.organization.name ]
            ]
        , p [] [ text package.description ]
        ]
