module Wizard.Pages.Public.Signup.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Form exposing (label)
import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Shared.Components.FormGroup as FormGroup
import Shared.Components.Page as Page
import Shared.Utils.Form.FormError exposing (FormError)
import String.Format as String
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Common.SignupForm exposing (SignupForm)
import Wizard.Pages.Public.Common.View exposing (publicForm)
import Wizard.Pages.Public.Routes exposing (Route(..))
import Wizard.Pages.Public.Signup.Models exposing (Model)
import Wizard.Pages.Public.Signup.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.signingUp of
                Success _ ->
                    Page.success <| gettext "Sign up was successful. Check your email for the activation link." appState.locale

                _ ->
                    signupForm appState model
    in
    div [ class "row justify-content-center Public__Signup" ]
        [ content ]


signupForm : AppState -> Model -> Html Msg
signupForm appState model =
    let
        formConfig =
            { title = gettext "Sign Up" appState.locale
            , submitMsg = FormMsg Form.Submit
            , actionResult = model.signingUp
            , submitLabel = gettext "Sign Up" appState.locale
            , formContent = formView appState model.form |> Html.map FormMsg
            , link = Just ( Routes.PublicRoute (LoginRoute Nothing), gettext "I already have an account" appState.locale )
            }
    in
    publicForm formConfig


formView : AppState -> Form FormError SignupForm -> Html Form.Msg
formView appState form =
    let
        acceptField =
            Form.getFieldAsBool "accept" form

        acceptFakeField =
            Form.getFieldAsBool "acceptFake" form

        hasError =
            case acceptField.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        viewAcceptGroup privacyText privacyError =
            div [ class "form-group form-group-accept", classList [ ( "has-error", hasError ) ] ]
                [ label [ for "accept" ]
                    (Input.checkboxInput acceptField [ id "accept", name "accept" ]
                        :: privacyText
                    )
                , p [ class "invalid-feedback" ] [ privacyError ]
                ]

        privacyLink privacyUrl =
            a [ href privacyUrl, target "_blank", dataCy "signup_link_privacy" ]
                [ text (gettext "Privacy" appState.locale) ]

        termsOfServiceLink termsOfServiceUrl =
            a [ href termsOfServiceUrl, target "_blank", dataCy "signup_link_tos" ]
                [ text (gettext "Terms of Service" appState.locale) ]

        acceptGroup =
            case ( appState.config.privacyAndSupport.privacyUrl, appState.config.privacyAndSupport.termsOfServiceUrl ) of
                ( Just privacyUrl, Just termsOfServiceUrl ) ->
                    viewAcceptGroup
                        (String.formatHtml
                            (gettext "I have read %s and %s." appState.locale)
                            [ privacyLink privacyUrl
                            , termsOfServiceLink termsOfServiceUrl
                            ]
                        )
                        (text
                            (String.format (gettext "You have to read %s and %s first" appState.locale)
                                [ gettext "Privacy" appState.locale
                                , gettext "Terms of Service" appState.locale
                                ]
                            )
                        )

                ( Just privacyUrl, Nothing ) ->
                    viewAcceptGroup
                        (String.formatHtml
                            (gettext "I have read %s." appState.locale)
                            [ privacyLink privacyUrl ]
                        )
                        (text
                            (String.format (gettext "You have to read %s first" appState.locale)
                                [ gettext "Privacy" appState.locale ]
                            )
                        )

                ( Nothing, Just termsOfServiceUrl ) ->
                    viewAcceptGroup
                        (String.formatHtml
                            (gettext "I have read %s." appState.locale)
                            [ termsOfServiceLink termsOfServiceUrl ]
                        )
                        (text
                            (String.format (gettext "You have to read %s first" appState.locale)
                                [ gettext "Terms of Service" appState.locale ]
                            )
                        )

                _ ->
                    Html.nothing

        acceptFakeGroup =
            div [ class "form-group form-group-accept2" ]
                [ label [ for "accept2" ]
                    [ Input.checkboxInput acceptFakeField [ id "accept2", name "accept2" ]
                    , text "I accept this."
                    ]
                ]
    in
    div []
        [ FormGroup.input appState.locale form "email" <| gettext "Email" appState.locale
        , FormGroup.input appState.locale form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState.locale form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.optionalWrapper appState.locale <|
            FormGroup.inputWithTypehints appState.config.organization.affiliations appState.locale form "affiliation" <|
                gettext "Affiliation" appState.locale
        , FormGroup.passwordWithStrength appState.locale form "password" <| gettext "Password" appState.locale
        , FormGroup.password appState.locale form "passwordConfirmation" <| gettext "Password again" appState.locale
        , acceptGroup
        , acceptFakeGroup
        ]
