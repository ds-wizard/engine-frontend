module Registry.Pages.Signup exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, hr, label, p, span, text)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Registry.Api.Organizations as Requests
import Registry.Components.ActionButton as ActionButton
import Registry.Components.FormGroup as FormGroup
import Registry.Components.FormResult as FormResult
import Registry.Components.FormWrapper as FormWrapper
import Registry.Components.Page as Page
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Forms.SignupForm as SignupForm exposing (SignupForm)
import Registry.Routes as Routes
import Shared.Components.Undraw as Undraw
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form as Form
import Shared.Utils.Form.FormError exposing (FormError)
import String.Format as String


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult ()
    }


initialModel : AppState -> Model
initialModel appState =
    { form = SignupForm.init appState
    , signingUp = ActionResult.Unset
    }


type Msg
    = FormMsg Form.Msg
    | PostOrganizationCompleted (Result ApiError ())


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just signupForm ) ->
                    ( { model | signingUp = ActionResult.Loading }
                    , Requests.postOrganization appState signupForm PostOrganizationCompleted
                    )

                _ ->
                    ( { model | form = Form.update (SignupForm.validation appState) formMsg model.form }
                    , Cmd.none
                    )

        PostOrganizationCompleted result ->
            case result of
                Ok _ ->
                    ( { model | signingUp = ActionResult.Success () }, Cmd.none )

                Err err ->
                    ( { model
                        | signingUp = ApiError.toActionResult appState (gettext "Registration was not successful." appState.locale) err
                        , form = Form.setFormErrors appState err model.form
                      }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    case model.signingUp of
        ActionResult.Success _ ->
            successView appState

        _ ->
            viewSignupForm appState model


successView : AppState -> Html Msg
successView appState =
    Page.illustratedMessage
        { image = Undraw.confirmation
        , heading = gettext "Sign up was successful!" appState.locale
        , msg = gettext "Check your email for the activation link." appState.locale
        }


viewSignupForm : AppState -> Model -> Html Msg
viewSignupForm appState model =
    let
        acceptField =
            Form.getFieldAsBool "accept" model.form

        hasError =
            case acceptField.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        acceptGroup =
            div [ class "form-group my-4 form-group-accept" ]
                [ label [ for "accept" ]
                    (Input.checkboxInput acceptField [ id "accept", name "accept", class "me-2" ]
                        :: String.formatHtml
                            (gettext "I have read %s and %s." appState.locale)
                            [ a [ href "https://ds-wizard.org/privacy.html", target "_blank" ]
                                [ text (gettext "Privacy" appState.locale) ]
                            , a [ href "https://ds-wizard.org/terms.html", target "_blank" ]
                                [ text (gettext "Terms of Service" appState.locale) ]
                            ]
                    )
                , p [ class "invalid-feedback", classList [ ( "d-block", hasError ) ] ] [ text (gettext "You have to read Privacy and Terms of Service first." appState.locale) ]
                ]
    in
    FormWrapper.view
        { title = gettext "Sign up" appState.locale
        , submitMsg = FormMsg Form.Submit
        , content =
            [ FormResult.errorOnlyView model.signingUp
            , Html.map FormMsg <| FormGroup.input appState model.form "organizationId" <| gettext "Organization ID" appState.locale
            , Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Organization Name" appState.locale
            , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
            , Html.map FormMsg <| FormGroup.textarea appState model.form "description" <| gettext "Organization Description" appState.locale
            , Html.map FormMsg <| acceptGroup
            , ActionButton.view
                { label = gettext "Sign up" appState.locale
                , actionResult = model.signingUp
                }
            , hr [] []
            , div [ class "text-center" ]
                [ span [ class "me-1" ] [ text (gettext "Already have an account?" appState.locale) ]
                , a [ href (Routes.toUrl Routes.login) ] [ text "Login" ]
                ]
            ]
        }
