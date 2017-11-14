module UserManagement.Edit.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import UserManagement.Edit.Models exposing (Model)
import UserManagement.Edit.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Edit user profile" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.user of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success user ->
            div []
                [ userView model
                , passwordView model
                ]


userView : Model -> Html Msgs.Msg
userView model =
    div [ class "well" ]
        [ legend [] [ text "Profile" ]
        , formResultView model.savingUser
        , userFormView model.userForm (model.uuid == "current")
        , formActionOnly ( "Save", model.savingUser, Msgs.UserManagementEditMsg <| EditFormMsg Form.Submit )
        ]


userFormView : Form () UserEditForm -> Bool -> Html Msgs.Msg
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
    formHtml |> Html.map (EditFormMsg >> Msgs.UserManagementEditMsg)


passwordView : Model -> Html Msgs.Msg
passwordView model =
    div [ class "well" ]
        [ legend [] [ text "Password" ]
        , formResultView model.savingPassword
        , passwordFormView model.passwordForm
        , formActionOnly ( "Save", model.savingPassword, Msgs.UserManagementEditMsg <| PasswordFormMsg Form.Submit )
        ]


passwordFormView : Form UserPasswordFormError UserPasswordForm -> Html Msgs.Msg
passwordFormView form =
    let
        formHtml =
            div []
                [ passwordGroup form "password" "New password"
                , passwordGroup form "passwordConfirmation" "New password again"
                ]
    in
    formHtml |> Html.map (PasswordFormMsg >> Msgs.UserManagementEditMsg)
