module Registry.Pages.Login exposing
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
import Html exposing (Html, a, div, hr, span, text)
import Html.Attributes exposing (class, href)
import Registry.Api.Models.Organization exposing (Organization)
import Registry.Api.Organizations as OrganizationsApi
import Registry.Components.ActionButton as ActionButton
import Registry.Components.FormGroup as FormGroup
import Registry.Components.FormResult as FormResult
import Registry.Components.FormWrapper as FormWrapper
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Forms.LoginForm as LoginForm exposing (LoginForm)
import Registry.Data.Session as Session exposing (Session)
import Registry.Routes as Routes
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
    FormWrapper.view
        { title = gettext "Login" appState.locale
        , submitMsg = FormMsg Form.Submit
        , content =
            [ FormResult.errorOnlyView model.loggingIn
            , Html.map FormMsg (FormGroup.input appState model.form "organizationId" (gettext "Organization ID" appState.locale))
            , Html.map FormMsg (FormGroup.password appState model.form "token" (gettext "Token" appState.locale))
            , ActionButton.view
                { label = gettext "Login" appState.locale
                , actionResult = model.loggingIn
                }
            , div [ class "text-end mt-2" ]
                [ a [ href (Routes.toUrl Routes.forgottenToken) ] [ text (gettext "Forgot your token?" appState.locale) ]
                ]
            , hr [] []
            , div [ class "text-center" ]
                [ span [ class "me-1" ] [ text (gettext "Need an account?" appState.locale) ]
                , a [ href (Routes.toUrl Routes.signup) ] [ text "Sign up" ]
                ]
            ]
        }
