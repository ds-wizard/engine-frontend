module UserManagement.Edit.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClass, emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing exposing (Route(UserManagement))
import UserManagement.Common.Models exposing (roles)
import UserManagement.Edit.Models exposing (..)
import UserManagement.Edit.Msgs exposing (Msg(..))
import UserManagement.Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClass ]
        [ pageHeader "Edit user profile" []
        , content wrapMsg model
        ]


content : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
content wrapMsg model =
    case model.user of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success user ->
            profileView wrapMsg model


profileView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
profileView wrapMsg model =
    let
        currentView =
            case model.currentView of
                Profile ->
                    userView wrapMsg model

                Password ->
                    passwordView wrapMsg model
    in
    div []
        [ navbar wrapMsg model
        , currentView
        ]


navbar : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
navbar wrapMsg model =
    let
        profileClass =
            if model.currentView == Profile then
                "active"
            else
                ""

        passwordClass =
            if model.currentView == Password then
                "active"
            else
                ""
    in
    ul [ class "nav nav-tabs" ]
        [ li [ class profileClass ]
            [ a [ onClick <| wrapMsg <| ChangeView Profile ]
                [ text "Profile" ]
            ]
        , li [ class passwordClass ]
            [ a [ onClick <| wrapMsg <| ChangeView Password ]
                [ text "Password" ]
            ]
        ]


userView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
userView wrapMsg model =
    div []
        [ formResultView model.savingUser
        , userFormView model.userForm (model.uuid == "current") |> Html.map (wrapMsg << EditFormMsg)
        , formActionsView model ( "Save", model.savingUser, wrapMsg <| EditFormMsg Form.Submit )
        ]


userFormView : Form CustomFormError UserEditForm -> Bool -> Html Form.Msg
userFormView form current =
    let
        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) roles

        roleSelect =
            if current then
                text ""
            else
                selectGroup roleOptions form "role" "Role"

        formHtml =
            div []
                [ inputGroup form "email" "Email"
                , inputGroup form "name" "Name"
                , inputGroup form "surname" "Surname"
                , roleSelect
                ]
    in
    formHtml


passwordView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
passwordView wrapMsg model =
    div []
        [ formResultView model.savingPassword
        , passwordFormView model.passwordForm |> Html.map (wrapMsg << PasswordFormMsg)
        , formActionsView model ( "Save", model.savingPassword, wrapMsg <| PasswordFormMsg Form.Submit )
        ]


passwordFormView : Form CustomFormError UserPasswordForm -> Html Form.Msg
passwordFormView form =
    div []
        [ passwordGroup form "password" "New password"
        , passwordGroup form "passwordConfirmation" "New password again"
        ]


formActionsView : Model -> ( String, ActionResult a, Msgs.Msg ) -> Html Msgs.Msg
formActionsView { uuid } actionButtonSettings =
    case uuid of
        "current" ->
            formActionOnly actionButtonSettings

        _ ->
            formActions (UserManagement UserManagement.Routing.Index) actionButtonSettings
