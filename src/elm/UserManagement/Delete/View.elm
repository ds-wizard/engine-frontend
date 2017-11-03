module UserManagement.Delete.View exposing (..)

import Common.Html exposing (defaultFullPageError, fullPageLoader, linkTo, onLinkClick, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg(..))
import Routing exposing (Route(..))
import UserManagement.Delete.Models exposing (Model)
import UserManagement.Delete.Msgs
import UserManagement.Models exposing (User)


view : Model -> Html Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Delete user" []
        , content model
        ]


content : Model -> Html Msg
content model =
    if model.error /= "" then
        defaultFullPageError model.error
    else if model.loadingUser then
        fullPageLoader
    else
        div []
            [ div
                [ class "alert alert-warning" ]
                [ text "You are about to permanently remove the following user from the portal." ]
            , maybeUser model.user
            , formActions model
            ]


formActions : Model -> Html Msg
formActions model =
    div [ class "form-actions" ]
        [ linkTo UserManagement [ class "btn btn-default" ] [ text "Cancel" ]
        , deleteButton model
        ]


deleteButton : Model -> Html Msg
deleteButton model =
    let
        buttonContent =
            if model.deletingUser then
                i [ class "fa fa-spinner fa-spin" ] []
            else
                text "Delete"
    in
    button
        [ class "btn btn-primary btn-with-loader"
        , disabled model.deletingUser
        , onLinkClick (UserManagementDeleteMsg UserManagement.Delete.Msgs.DeleteUser)
        ]
        [ buttonContent ]


maybeUser : Maybe User -> Html Msg
maybeUser maybeUser =
    case maybeUser of
        Just user ->
            userCard user

        Nothing ->
            text ""


userCard : User -> Html Msg
userCard user =
    div [ class "user-card" ]
        [ div [ class "icon" ] [ i [ class "fa fa-user-circle-o" ] [] ]
        , div [ class "name" ] [ text (user.name ++ " " ++ user.surname) ]
        , div [ class "email" ]
            [ a [ href ("mailto:" ++ user.email) ] [ text user.email ]
            ]
        , div [ class "role" ]
            [ text ("Role: " ++ user.role)
            ]
        ]
