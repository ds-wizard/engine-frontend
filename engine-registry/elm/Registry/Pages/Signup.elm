module Registry.Pages.Signup exposing
    ( Model
    , Msg
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
import Html exposing (Html, a, div, form, label, p, text)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.FormExtra exposing (CustomFormError, setFormErrors)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Registry.Common.View.Page as Page
import Registry.Utils exposing (validateRegex)
import Result exposing (Result)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lh, lx)


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.Signup"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.Signup"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Registry.Pages.Signup"


init : Model
init =
    { form = initSignupForm
    , signingUp = Unset
    }



-- MODEL


type alias Model =
    { form : Form CustomFormError SignupForm
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
                        | signingUp = ApiError.toActionResult (l_ "update.postError" appState) err
                        , form = setFormErrors err model.form
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
        { image = "confirmation"
        , heading = l_ "success.heading" appState
        , msg = l_ "success.msg" appState
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
                    ([ Input.checkboxInput acceptField [ id "accept", name "accept" ] ]
                        ++ lh_ "formView.privacyRead"
                            [ a [ href "https://ds-wizard.org/privacy.html", target "_blank" ]
                                [ lx_ "formView.privacy" appState ]
                            ]
                            appState
                    )
                , p [ class "invalid-feedback" ] [ lx_ "formView.privacyError" appState ]
                ]
    in
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ lx_ "formView.title" appState ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.signingUp
                , Html.map FormMsg <| FormGroup.input appState model.form "organizationId" <| l_ "formView.organizationId" appState
                , Html.map FormMsg <| FormGroup.input appState model.form "name" <| l_ "formView.name" appState
                , Html.map FormMsg <| FormGroup.input appState model.form "email" <| l_ "formView.email" appState
                , Html.map FormMsg <| FormGroup.textarea appState model.form "description" <| l_ "formView.description" appState
                , Html.map FormMsg <| acceptGroup
                , ActionButton.submit ( l_ "formView.signUp" appState, model.signingUp )
                ]
            ]
        ]
