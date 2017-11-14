module UserManagement.Delete.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Common.View.Forms exposing (formActions, formResultView)
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
    case model.user of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success user ->
            div []
                [ formResultView model.deletingUser
                , div
                    [ class "alert alert-warning" ]
                    [ text "You are about to permanently remove the following user from the portal." ]
                , userCard user
                , formActions UserManagement ( "Delete", model.deletingUser, UserManagementDeleteMsg UserManagement.Delete.Msgs.DeleteUser )
                ]


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
