module UserManagement.Delete.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (formActions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
            , formActions UserManagement ( "Delete", model.deletingUser, UserManagementDeleteMsg UserManagement.Delete.Msgs.DeleteUser )
            ]


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
