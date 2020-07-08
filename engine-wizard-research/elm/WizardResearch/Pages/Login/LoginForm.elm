module WizardResearch.Pages.Login.LoginForm exposing
    ( LoginForm
    , encode
    , init
    , validation
    , view
    )

import ActionResult exposing (ActionResult)
import Css exposing (important, marginBottom, minHeight, minWidth, paddingLeft, paddingRight, zero)
import Css.Global exposing (class, descendants, selector)
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as V exposing (Validation)
import Html.Attributes exposing (name)
import Html.Styled exposing (Html, a, br, button, div, form, fromUnstyled, label, span, text)
import Html.Styled.Attributes exposing (css, for, href, placeholder, type_)
import Html.Styled.Events exposing (onSubmit)
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Atoms.Form as Form
import Shared.Elemental.Atoms.FormInput as FormInput
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html.Styled exposing (fa)
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Route as Route



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


view : AppState -> Form FormError LoginForm -> ActionResult a -> Html Form.Msg
view appState loginForm submitActionResult =
    let
        emailFormGroup =
            Form.groupSimple
                { input = FormInput.textWithAttrs [ placeholder "Type your email" ]
                , toMsg = identity
                }
                { form = loginForm
                , fieldName = "email"
                , fieldReadableName = "Email"
                , mbFieldLabel = Nothing
                , mbTextBefore = Nothing
                , mbTextAfter = Nothing
                }

        passwordFormGroup =
            Form.groupSimple
                { input = FormInput.passwordWithAttrs [ placeholder "Type your password" ]
                , toMsg = identity
                }
                { form = loginForm
                , fieldName = "password"
                , fieldReadableName = "Password"
                , mbFieldLabel = Nothing
                , mbTextBefore = Nothing
                , mbTextAfter = Nothing
                }

        styles =
            [ descendants
                [ selector ".form-group:first-child"
                    [ important (marginBottom zero)
                    ]
                ]
            ]

        buttonStyles =
            [ paddingLeft (px2rem Spacing.lg)
            , paddingRight (px2rem Spacing.lg)
            , minWidth (px2rem 150)
            , minHeight (px2rem 37)
            ]

        separatorStyles =
            [ Typography.copy1light appState.theme
            , paddingLeft (px2rem Spacing.sm)
            , paddingRight (px2rem Spacing.sm)
            ]
    in
    form [ css styles, onSubmit Form.Submit ]
        [ emailFormGroup appState
        , passwordFormGroup appState
        , div [ css [ Spacing.stackLG ] ]
            [ Button.primaryLoader appState.theme
                submitActionResult
                [ css buttonStyles, type_ "submit" ]
                [ span [] [ text "Log in" ]
                , fa "fas fa-long-arrow-alt-right"
                ]
            ]
        , div [ css [ Spacing.stackLG ] ]
            [ a [ href (Route.toString Route.ForgottenPassword) ] [ text "Forgot your password?" ]
            , span [ css separatorStyles ] [ text "|" ]
            , a [ href (Route.toString Route.SignUp) ] [ text "Sign up" ]
            ]
        ]
