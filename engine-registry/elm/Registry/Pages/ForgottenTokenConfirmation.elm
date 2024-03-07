module Registry.Pages.ForgottenTokenConfirmation exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, h5, p, strong, text)
import Html.Attributes exposing (class, href)
import Registry.Api.Models.Organization exposing (Organization)
import Registry.Api.Organizations as OrganizationsApi
import Registry.Components.FontAwesome exposing (fas)
import Registry.Components.Page as Page
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String.Format as String


type alias Model =
    { organization : ActionResult Organization }


initialModel : Model
initialModel =
    { organization = ActionResult.Loading }


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState organizationId hash =
    ( initialModel
    , OrganizationsApi.putOrganizationToken appState
        { organizationId = organizationId
        , hash = hash
        }
        PutOrganizationTokenCompleted
    )


setOrganization : ActionResult Organization -> Model -> Model
setOrganization organization model =
    { model | organization = organization }


type Msg
    = PutOrganizationTokenCompleted (Result ApiError Organization)


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        PutOrganizationTokenCompleted result ->
            ActionResult.apply setOrganization (ApiError.toActionResult appState (gettext "Unable to recover your organization token." appState.locale)) result


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewOrganization appState) model.organization


viewOrganization : AppState -> Organization -> Html Msg
viewOrganization appState organization =
    div [ class "d-flex justify-content-center align-items-center my-5" ]
        [ div [ class "bg-white rounded shadow-sm p-4 w-100 box box-wide" ]
            [ h5 [ class "text-success" ]
                [ fas "fa-check me-2"
                , text (gettext "Recovered" appState.locale)
                ]
            , p []
                (String.formatHtml
                    (gettext "A new token for your organization %s has been generated!" appState.locale)
                    [ strong [] [ text organization.name ] ]
                )
            , div [ class "alert alert-info" ]
                [ text (gettext "You will use the following token for authentication. Save it to a safe place." appState.locale)
                , strong [ class "d-block mt-2" ] [ text (gettext "You will not be able to see it again." appState.locale) ]
                ]
            , div [ class "card" ]
                [ div [ class "card-header" ] [ text (gettext "Token" appState.locale) ]
                , div [ class "card-body" ] [ code [] [ text organization.token ] ]
                ]
            , a
                [ class "btn btn-primary w-100 mt-4"
                , href (Routes.toUrl Routes.login)
                ]
                [ text (gettext "Login" appState.locale) ]
            ]
        ]
