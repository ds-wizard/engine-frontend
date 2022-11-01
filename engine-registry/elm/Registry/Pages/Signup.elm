module Registry.Pages.Signup exposing
    ( Model
    , Msg
    , SignupForm
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, form, label, p, text)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Registry.Common.View.Page as Page
import Registry.Utils exposing (validateRegex)
import Result exposing (Result)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Shared.Form.FormError exposing (FormError)
import Shared.Undraw as Undraw
import String.Format as String


init : Model
init =
    { form = initSignupForm
    , signingUp = Unset
    }



-- MODEL


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult ()
    }


type alias SignupForm =
    { organizationId : String
    , name : String
    , email : String
    , description : String
    , accept : Bool
    }


signupFormValidation : Validation e SignupForm
signupFormValidation =
    Validate.map5 SignupForm
        (Validate.field "organizationId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)
        (Validate.field "description" Validate.string)
        (Validate.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True

    else
        Err (Error.value Empty)


initSignupForm : Form e SignupForm
initSignupForm =
    Form.initial [] signupFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PostOrganizationCompleted (Result ApiError ())


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostOrganizationCompleted result ->
            case result of
                Ok _ ->
                    ( { model | signingUp = Success () }, Cmd.none )

                Err err ->
                    ( { model
                        | signingUp = ApiError.toActionResult appState (gettext "Registration was not successful." appState.locale) err
                        , form = setFormErrors appState err model.form
                      }
                    , Cmd.none
                    )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            ( { model | signingUp = Loading }
            , Requests.postOrganization signupForm appState PostOrganizationCompleted
            )

        _ ->
            ( { model | form = Form.update signupFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "Signup" ]
        [ if ActionResult.isSuccess model.signingUp then
            successView appState

          else
            formView appState model
        ]


successView : AppState -> Html Msg
successView appState =
    Page.illustratedMessage
        { image = Undraw.confirmation
        , heading = gettext "Sign up was successful!" appState.locale
        , msg = gettext "Check your email address for the activation link." appState.locale
        }


formView : AppState -> Model -> Html Msg
formView appState model =
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
            div [ class "form-group form-group-accept", classList [ ( "has-error", hasError ) ] ]
                [ label [ for "accept" ]
                    (Input.checkboxInput acceptField [ id "accept", name "accept" ]
                        :: String.formatHtml
                            (gettext "I have read %s and %s." appState.locale)
                            [ a [ href "https://ds-wizard.org/privacy.html", target "_blank" ]
                                [ text (gettext "Privacy" appState.locale) ]
                            , a [ href "https://ds-wizard.org/terms.html", target "_blank" ]
                                [ text (gettext "Terms of Service" appState.locale) ]
                            ]
                    )
                , p [ class "invalid-feedback" ] [ text (gettext "You have to read Privacy and Terms of Service first." appState.locale) ]
                ]
    in
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text (gettext "Sign Up" appState.locale) ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.signingUp
                , Html.map FormMsg <| FormGroup.input appState model.form "organizationId" <| gettext "Organization ID" appState.locale
                , Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Organization Name" appState.locale
                , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
                , Html.map FormMsg <| FormGroup.textarea appState model.form "description" <| gettext "Organization Description" appState.locale
                , Html.map FormMsg <| acceptGroup
                , ActionButton.submit ( gettext "Sign Up" appState.locale, model.signingUp )
                ]
            ]
        ]
