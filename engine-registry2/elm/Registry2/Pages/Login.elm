module Registry2.Pages.Login exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, form, h5, hr, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onSubmit)
import Registry2.Api.Models.Organization exposing (Organization)
import Registry2.Api.Organizations as OrganizationsApi
import Registry2.Components.ActionButton as ActionButton
import Registry2.Components.FormGroup as FormGroup
import Registry2.Components.FormResult as FormResult
import Registry2.Data.AppState exposing (AppState)
import Registry2.Data.Forms.LoginForm as LoginForm exposing (LoginForm)
import Registry2.Data.Session as Session exposing (Session)
import Registry2.Routes as Routes
import Shared.Error.ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils as Task


type alias Model =
    { form : Form FormError LoginForm
    , loggingIn : ActionResult ()
    }


initialModel : Model
initialModel =
    { form = LoginForm.init
    , loggingIn = ActionResult.Unset
    }


type Msg
    = FormMsg Form.Msg
    | GetOrganizationCompleted (Result ApiError Organization)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , setSessionMsg : Maybe Session -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just loginForm ) ->
                    let
                        appStateWithSession =
                            { appState | session = Just <| Session.setToken loginForm.token <| Session.init }
                    in
                    ( { model | loggingIn = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <|
                        OrganizationsApi.getOrganization appStateWithSession loginForm.organizationId GetOrganizationCompleted
                    )

                _ ->
                    ( { model | form = Form.update LoginForm.validation formMsg model.form }
                    , Cmd.none
                    )

        GetOrganizationCompleted result ->
            case result of
                Ok organization ->
                    ( model
                    , Task.dispatch (cfg.setSessionMsg <| Just <| Session.fromOrganization organization)
                    )

                Err _ ->
                    ( { model | loggingIn = ActionResult.Error (gettext "Login failed." appState.locale) }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "d-flex justify-content-center align-items-center my-5" ]
        [ form
            [ class "bg-white rounded shadow-sm p-4 w-100 public-form"
            , onSubmit (FormMsg Form.Submit)
            ]
            [ h5 [] [ text "Login" ]
            , FormResult.errorOnlyView model.loggingIn
            , Html.map FormMsg (FormGroup.input appState model.form "organizationId" (gettext "Organization ID" appState.locale))
            , Html.map FormMsg (FormGroup.password appState model.form "token" (gettext "Token" appState.locale))
            , ActionButton.view
                { label = gettext "Login" appState.locale
                , actionResult = model.loggingIn
                }
            , div [ class "text-end mt-2" ]
                [ a [ href (Routes.toUrl Routes.forgottenToken) ] [ text "Forgot your token?" ]
                ]
            , hr [] []
            , div [ class "text-center" ]
                [ span [ class "me-1" ] [ text "Need an account?" ]
                , a [ href (Routes.toUrl Routes.signup) ] [ text "Sign up" ]
                ]
            ]
        ]
