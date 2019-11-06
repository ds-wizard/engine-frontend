module Registry.Pages.ForgottenTokenConfirmation exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, h1, p, strong, text)
import Html.Attributes exposing (class)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lh, lx)


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.ForgottenTokenConfirmation"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.ForgottenTokenConfirmation"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Registry.Pages.ForgottenTokenConfirmation"


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState organizationId hash =
    ( { organization = Loading }
    , Requests.putOrganizationToken
        { organizationId = organizationId
        , hash = hash
        }
        appState
        PutOrganizationTokenCompleted
    )



-- MODEL


type alias Model =
    { organization : ActionResult OrganizationDetail }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    { model | organization = organization }



-- UPDATE


type Msg
    = PutOrganizationTokenCompleted (Result ApiError OrganizationDetail)


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        PutOrganizationTokenCompleted result ->
            ActionResult.apply setOrganization
                (ApiError.toActionResult (l_ "update.putError" appState))
                result



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewOrganization appState) model.organization


viewOrganization : AppState -> OrganizationDetail -> Html Msg
viewOrganization appState organization =
    div []
        [ h1 [] [ lx_ "view.title" appState ]
        , p [] (lh_ "view.text" [ strong [] [ text organization.name ] ] appState)
        , div [ class "alert alert-info" ]
            [ lx_ "view.info" appState ]
        , div [ class "card" ]
            [ div [ class "card-header" ] [ lx_ "view.token" appState ]
            , div [ class "card-body" ] [ text organization.token ]
            ]
        ]
