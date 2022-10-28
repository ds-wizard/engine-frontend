module Registry.Pages.ForgottenTokenConfirmation exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, strong, text)
import Html.Attributes exposing (class)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String.Format as String


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
                (ApiError.toActionResult appState (gettext "Unable to recover your organization token." appState.locale))
                result



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewOrganization appState) model.organization


viewOrganization : AppState -> OrganizationDetail -> Html Msg
viewOrganization appState organization =
    div []
        [ h1 [] [ text (gettext "Recovered" appState.locale) ]
        , p []
            (String.formatHtml
                (gettext "A new token for your organization %s has been generated!" appState.locale)
                [ strong [] [ text organization.name ] ]
            )
        , div [ class "alert alert-info" ]
            [ text (gettext "You will use the following token for authentication. Save it to a safe place. You will not be able to see it again." appState.locale) ]
        , div [ class "card" ]
            [ div [ class "card-header" ] [ text (gettext "Token" appState.locale) ]
            , div [ class "card-body" ] [ text organization.token ]
            ]
        ]
