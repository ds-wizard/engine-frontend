module Registry.Pages.DocumentTemplates exposing
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
import Registry.Common.Entities.DocumentTemplate exposing (DocumentTemplate)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Error.ApiError as ApiError exposing (ApiError)


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( { documentTemplates = Loading }
    , Requests.getDocumentTemplates appState GetDocumentTemplatesCompleted
    )



-- MODEL


type alias Model =
    { documentTemplates : ActionResult (List DocumentTemplate) }


setPackages : ActionResult (List DocumentTemplate) -> Model -> Model
setPackages documentTemplates model =
    { model | documentTemplates = documentTemplates }



-- UPDATE


type Msg
    = GetDocumentTemplatesCompleted (Result ApiError (List DocumentTemplate))


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        GetDocumentTemplatesCompleted result ->
            ActionResult.apply setPackages (ApiError.toActionResult appState (gettext "Unable to get templates." appState.locale)) result



-- VIEW


view : Model -> Html Msg
view model =
    Page.actionResultView viewList model.documentTemplates


viewList : List DocumentTemplate -> Html Msg
viewList documentTemplates =
    div []
        [ div [ class "list-group list-group-flush" ]
            (List.map viewItem <| List.sortBy .name documentTemplates)
        ]


viewItem : DocumentTemplate -> Html Msg
viewItem documentTemplate =
    let
        packageLink =
            Routing.toString <| Routing.DocumentTemplateDetail documentTemplate.id
    in
    div [ class "list-group-item flex-column align-items-start" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-1" ]
                [ a [ href packageLink ] [ text documentTemplate.name ]
                ]
            , small [] [ text documentTemplate.organization.name ]
            ]
        , p [] [ text documentTemplate.description ]
        ]
