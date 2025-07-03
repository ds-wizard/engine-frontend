module Wizard.Public.Auth.View exposing (view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, a, div, form, input, label, span, text)
import Html.Attributes exposing (checked, class, disabled, href, target, type_)
import Html.Events exposing (onCheck, onSubmit)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    if Maybe.isJust model.hash then
        viewConsentForm appState model

    else
        Page.actionResultView appState (\_ -> Html.nothing) model.authenticating


viewConsentForm : AppState -> Model -> Html Msg
viewConsentForm appState model =
    let
        privacyLink privacyUrl =
            a [ href privacyUrl, target "_blank", dataCy "signup_link_privacy" ]
                [ text (gettext "Privacy" appState.locale) ]

        termsOfServiceLink termsOfServiceUrl =
            a [ href termsOfServiceUrl, target "_blank", dataCy "signup_link_tos" ]
                [ text (gettext "Terms of Service" appState.locale) ]

        consentLabel =
            case ( appState.config.privacyAndSupport.privacyUrl, appState.config.privacyAndSupport.termsOfServiceUrl ) of
                ( Just privacyUrl, Just termsOfServiceUrl ) ->
                    String.formatHtml
                        (gettext "I have read %s and %s." appState.locale)
                        [ privacyLink privacyUrl
                        , termsOfServiceLink termsOfServiceUrl
                        ]

                ( Just privacyUrl, Nothing ) ->
                    String.formatHtml
                        (gettext "I have read %s." appState.locale)
                        [ privacyLink privacyUrl ]

                ( Nothing, Just termsOfServiceUrl ) ->
                    String.formatHtml
                        (gettext "I have read %s." appState.locale)
                        [ termsOfServiceLink termsOfServiceUrl ]

                _ ->
                    []
    in
    div [ class "row" ]
        [ div [ class "col-xl-4 col-lg-5 col-md-6 col-sm-8 mx-auto" ]
            [ form [ class "card bg-light", onSubmit SubmitConsent ]
                [ div [ class "card-body" ]
                    [ FormResult.view model.submittingConsent
                    , div [ class "form-group" ]
                        [ div [ class "form-check" ]
                            [ label [ class "form-check-label form-check-toggle fw-normal" ]
                                [ input [ type_ "checkbox", checked model.consent, onCheck CheckConsent ] []
                                , span [] consentLabel
                                ]
                            ]
                        ]
                    , div [ class "form-group" ]
                        [ ActionButton.submitWithAttrs
                            { label = gettext "Continue" appState.locale
                            , result = ActionResult.Unset
                            , attrs = [ class "w-100", disabled (not model.consent) ]
                            }
                        ]
                    ]
                ]
            ]
        ]
