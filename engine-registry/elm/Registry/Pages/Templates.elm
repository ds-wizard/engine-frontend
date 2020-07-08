module Registry.Pages.Templates exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, a, div, h5, p, small, text)
import Html.Attributes exposing (class, href)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.Template exposing (Template)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l)


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.Templates"


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { templates = Loading }
    , Requests.getTemplates appState GetTemplatesCompleted
    )



-- MODEL


type alias Model =
    { templates : ActionResult (List Template) }


setPackages : ActionResult (List Template) -> Model -> Model
setPackages templates model =
    { model | templates = templates }



-- UPDATE


type Msg
    = GetTemplatesCompleted (Result ApiError (List Template))


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        GetTemplatesCompleted result ->
            ActionResult.apply setPackages (ApiError.toActionResult (l_ "update.getError" appState)) result



-- VIEW


view : Model -> Html Msg
view model =
    Page.actionResultView viewList model.templates


viewList : List Template -> Html Msg
viewList templates =
    div []
        [ div [ class "list-group list-group-flush" ]
            (List.map viewItem <| List.sortBy .name templates)
        ]


viewItem : Template -> Html Msg
viewItem template =
    let
        packageLink =
            Routing.toString <| Routing.TemplateDetail template.id
    in
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href packageLink ] [ text template.name ]
                ]
            , small [] [ text template.organization.name ]
            ]
        , p [] [ text template.description ]
        ]
