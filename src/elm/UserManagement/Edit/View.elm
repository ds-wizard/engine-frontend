module UserManagement.Edit.View exposing (..)

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
    if model.loading then
        fullPageLoader
    else if model.loadingError /= "" then
        defaultFullPageError model.loadingError
    else
        div []
            [ editView model
            , passwordView model
            ]


editView : Model -> Html Msgs.Msg
editView model =
    div [ class "well" ]
        [ legend [] [ text "Profile" ]
        , errorView model.editError
        , editFormView model.editForm (model.uuid == "current")
        , formActionOnly ( "Save", model.editSaving, Msgs.UserManagementEditMsg <| EditFormMsg Form.Submit )
        ]


editFormView : Form () UserEditForm -> Bool -> Html Msgs.Msg
editFormView form current =
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
        , errorView model.passwordError
        , passwordFormView model.passwordForm
        , formActionOnly ( "Save", model.passwordSaving, Msgs.UserManagementEditMsg <| PasswordFormMsg Form.Submit )
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
