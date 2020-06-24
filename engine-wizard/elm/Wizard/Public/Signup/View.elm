module Wizard.Public.Signup.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lg, lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Public.Common.SignupForm exposing (SignupForm)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.Models exposing (Model)
import Wizard.Public.Signup.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.Signup.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Public.Signup.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Public.Signup.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.signingUp of
                Success _ ->
                    Page.success appState <| l_ "success" appState

                _ ->
                    signupForm appState model
    in
    div [ class "row justify-content-center Public__Signup" ]
        [ content ]


signupForm : AppState -> Model -> Html Msg
signupForm appState model =
    let
        formConfig =
            { title = l_ "form.title" appState
            , submitMsg = FormMsg Form.Submit
            , actionResult = model.signingUp
            , submitLabel = l_ "form.submit" appState
            , formContent = formView appState model.form |> Html.map FormMsg
            , link = Just ( Routes.PublicRoute (LoginRoute Nothing), l_ "form.link" appState )
            }
    in
    publicForm appState formConfig


formView : AppState -> Form FormError SignupForm -> Html Form.Msg
formView appState form =
    let
        acceptField =
            Form.getFieldAsBool "accept" form

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
                        ++ lh_ "form.privacyText"
                            [ a [ href <| PrivacyAndSupportConfig.getPrivacyUrl appState.config.privacyAndSupport, target "_blank" ]
                                [ lx_ "form.privacy" appState ]
                            ]
                            appState
                    )
                , p [ class "invalid-feedback" ] [ lx_ "form.privacyError" appState ]
                ]
    in
    div []
        [ FormGroup.input appState form "email" <| lg "user.email" appState
        , FormGroup.input appState form "firstName" <| lg "user.firstName" appState
        , FormGroup.input appState form "lastName" <| lg "user.lastName" appState
        , FormGroup.optionalWrapper appState <|
            FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <|
                lg "user.affiliation" appState
        , FormGroup.password appState form "password" <| lg "user.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| lg "user.passwordConfirmation" appState
        , acceptGroup
        ]
