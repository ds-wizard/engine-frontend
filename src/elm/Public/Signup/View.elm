module Public.Signup.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Locale exposing (l, lg, lh, lx)
import Common.View.FormGroup as FormGroup
import Common.View.Page as Page
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, classList, for, href, id, name, target)
import Public.Common.SignupForm exposing (SignupForm)
import Public.Common.View exposing (publicForm)
import Public.Routes exposing (Route(..))
import Public.Signup.Models exposing (Model)
import Public.Signup.Msgs exposing (Msg(..))
import Routes


l_ : String -> AppState -> String
l_ =
    l "Public.Signup.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Public.Signup.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Public.Signup.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.signingUp of
                Success _ ->
                    Page.success <| l_ "success" appState

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
            , link = Just ( Routes.PublicRoute LoginRoute, l_ "form.link" appState )
            }
    in
    publicForm appState formConfig


formView : AppState -> Form CustomFormError SignupForm -> Html Form.Msg
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
                            [ a [ href appState.config.client.privacyUrl, target "_blank" ]
                                [ lx_ "form.privacy" appState ]
                            ]
                            appState
                    )
                , p [ class "invalid-feedback" ] [ lx_ "form.privacyError" appState ]
                ]
    in
    div []
        [ FormGroup.input appState form "email" <| lg "user.email" appState
        , FormGroup.input appState form "name" <| lg "user.name" appState
        , FormGroup.input appState form "surname" <| lg "user.surname" appState
        , FormGroup.password appState form "password" <| lg "user.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| lg "user.passwordConfirmation" appState
        , acceptGroup
        ]
