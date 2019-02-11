module Users.Edit.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Html exposing (detailContainerClassWith, emptyNode)
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing exposing (Route(..))
import Users.Common.Models exposing (User, roles)
import Users.Edit.Models exposing (..)
import Users.Edit.Msgs exposing (Msg(..))
import Users.Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "Users__Edit" ]
        [ Page.header "Edit user profile" []
        , Page.actionResultView (profileView wrapMsg model) model.user
        ]


profileView : (Msg -> Msgs.Msg) -> Model -> User -> Html Msgs.Msg
profileView wrapMsg model _ =
    div []
        [ navbar wrapMsg model
        , userView wrapMsg model
        , passwordView wrapMsg model
        ]


navbar : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
navbar wrapMsg model =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.currentView == Profile ) ]
                , onClick <| wrapMsg <| ChangeView Profile
                ]
                [ text "Profile" ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.currentView == Password ) ]
                , onClick <| wrapMsg <| ChangeView Password
                ]
                [ text "Password" ]
            ]
        ]


userView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
userView wrapMsg model =
    div [ class <| getClass (model.currentView /= Profile) "hidden" ]
        [ FormResult.view model.savingUser
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
                emptyNode

            else
                FormGroup.select roleOptions form "role" "Role"

        activeToggle =
            if current then
                emptyNode

            else
                FormGroup.toggle form "active" "Active"

        formHtml =
            div []
                [ FormGroup.input form "email" "Email"
                , FormGroup.input form "name" "Name"
                , FormGroup.input form "surname" "Surname"
                , roleSelect
                , activeToggle
                ]
    in
    formHtml


passwordView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
passwordView wrapMsg model =
    div [ class <| getClass (model.currentView /= Password) "hidden" ]
        [ FormResult.view model.savingPassword
        , passwordFormView model.passwordForm |> Html.map (wrapMsg << PasswordFormMsg)
        , formActionsView model ( "Save", model.savingPassword, wrapMsg <| PasswordFormMsg Form.Submit )
        ]


passwordFormView : Form CustomFormError UserPasswordForm -> Html Form.Msg
passwordFormView form =
    div []
        [ FormGroup.password form "password" "New password"
        , FormGroup.password form "passwordConfirmation" "New password again"
        ]


formActionsView : Model -> ( String, ActionResult a, Msgs.Msg ) -> Html Msgs.Msg
formActionsView { uuid } actionButtonSettings =
    case uuid of
        "current" ->
            FormActions.viewActionOnly actionButtonSettings

        _ ->
            FormActions.view (Users Users.Routing.Index) actionButtonSettings


getClass : Bool -> String -> String
getClass condition class =
    if condition then
        class

    else
        ""
