module Registry.Pages.SignupConfirmation exposing
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
    l "Registry.Pages.SignupConfirmation"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.SignupConfirmation"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Registry.Pages.SignupConfirmation"


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState organizationId hash =
    ( { organization = Loading }
    , Requests.putOrganizationState
        { organizationId = organizationId
        , hash = hash
        , active = True
        }
        appState
        PutOrganizationStateCompleted
    )



-- MODEL


type alias Model =
    { organization : ActionResult OrganizationDetail }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    { model | organization = organization }



-- UPDATE


type Msg
    = PutOrganizationStateCompleted (Result ApiError OrganizationDetail)


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        PutOrganizationStateCompleted result ->
            ActionResult.apply setOrganization (ApiError.toActionResult appState (l_ "update.putError" appState)) result



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewOrganization appState) model.organization


viewOrganization : AppState -> OrganizationDetail -> Html Msg
viewOrganization appState organization =
    div []
        [ h1 [] [ lx_ "viewOrganization.title" appState ]
        , p []
            (lh_ "viewOrganization.activated" [ strong [] [ text organization.name ] ] appState)
        , div [ class "alert alert-info" ]
            [ lx_ "viewOrganization.tokenInfo" appState ]
        , div [ class "card" ]
            [ div [ class "card-header" ] [ lx_ "viewOrganization.token" appState ]
            , div [ class "card-body" ] [ text organization.token ]
            ]
        ]
