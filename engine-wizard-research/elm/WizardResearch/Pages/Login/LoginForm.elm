module WizardResearch.Pages.Login.LoginForm exposing
    ( LoginForm
    , encode
    , init
    , validation
    , view
    )

import Form exposing (Form)
import Form.Input as Input
import Form.Validate as V exposing (Validation)
import Html.Attributes exposing (name)
import Html.Styled exposing (Html, br, button, form, fromUnstyled, label, text)
import Html.Styled.Attributes exposing (for, type_)
import Html.Styled.Events exposing (onSubmit)
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import WizardResearch.Common.AppState exposing (AppState)



-- MODEL


type alias LoginForm =
    { email : String
    , password : String
    }


validation : Validation FormError LoginForm
validation =
    V.succeed LoginForm
        |> V.andMap (V.field "email" V.string)
        |> V.andMap (V.field "password" V.string)


init : Form FormError LoginForm
init =
    Form.initial [] validation


encode : LoginForm -> E.Value
encode form =
    E.object
        [ ( "email", E.string form.email )
        , ( "password", E.string form.password )
        ]



-- VIEW


view : AppState -> Form FormError LoginForm -> Html Form.Msg
view appState loginForm =
    let
        emailField =
            Form.getFieldAsString "email" loginForm

        emailError =
            Maybe.unwrap "" (Form.errorToString appState "Email") emailField.liveError

        passwordField =
            Form.getFieldAsString "password" loginForm

        passwordError =
            Maybe.unwrap "" (Form.errorToString appState "Password") passwordField.liveError
    in
    form [ onSubmit Form.Submit ]
        [ label [ for "email" ] [ text "Email" ]
        , fromUnstyled <| Input.textInput emailField [ name "email" ]
        , text emailError
        , br [] []
        , label [ for "email" ] [ text "Password" ]
        , fromUnstyled <| Input.passwordInput passwordField [ name "password" ]
        , text passwordError
        , br [] []
        , button [ type_ "submit" ] [ text "Log in" ]
        ]
