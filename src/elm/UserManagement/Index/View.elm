module UserManagement.Index.View exposing (view)

{-|

@docs view

-}

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing exposing (Route(..))
import UserManagement.Index.Models exposing (..)
import UserManagement.Index.Msgs exposing (Msg(..))
import UserManagement.Models exposing (User)


{-| -}
view : Model -> Html Msgs.Msg
view model =
    div [ class "user-management" ]
        [ pageHeader "User Management" indexActions
        , formSuccessResultView model.deletingUser
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.users of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success users ->
            umTable model users


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo UserManagementCreate [ class "btn btn-primary" ] [ text "Create User" ] ]


umTable : Model -> List User -> Html Msgs.Msg
umTable model users =
    table [ class "table" ]
        [ umTableHeader
        , umTableBody users
        , deleteModal model
        ]


umTableHeader : Html Msgs.Msg
umTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Surname" ]
            , th [] [ text "Email" ]
            , th [] [ text "Role" ]
            , th [] [ text "Actions" ]
            ]
        ]


umTableBody : List User -> Html Msgs.Msg
umTableBody users =
    if List.isEmpty users then
        umTableEmpty
    else
        tbody [] (List.map umTableRow users)


umTableEmpty : Html msg
umTableEmpty =
    tr []
        [ td [ colspan 5, class "td-empty-table" ] [ text "There are no users." ] ]


umTableRow : User -> Html Msgs.Msg
umTableRow user =
    tr []
        [ td [] [ text user.name ]
        , td [] [ text user.surname ]
        , td [] [ text user.email ]
        , td [] [ text user.role ]
        , td [ class "table-actions" ] [ umTableRowActionDelete user, umTableRowActionEdit user ]
        ]


umTableRowActionDelete : User -> Html Msgs.Msg
umTableRowActionDelete user =
    a [ onClick <| Msgs.UserManagementIndexMsg <| ShowHideDeleteUser <| Just user ]
        [ i [ class "fa fa-trash-o" ] [] ]


umTableRowActionEdit : User -> Html Msgs.Msg
umTableRowActionEdit user =
    linkTo (UserManagementEdit user.uuid) [] [ i [ class "fa fa-edit" ] [] ]


deleteModal : Model -> Html Msgs.Msg
deleteModal model =
    let
        ( visible, userHtml ) =
            case model.userToBeDeleted of
                Just user ->
                    ( True, userCard user )

                Nothing ->
                    ( False, emptyNode )

        modalContent =
            [ p []
                [ text "Are you sure you want to permamently delete the following user?" ]
            , userHtml
            ]

        modalConfig =
            { modalTitle = "Delete user"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingUser
            , actionName = "Delete"
            , actionMsg = Msgs.UserManagementIndexMsg DeleteUser
            , cancelMsg = Msgs.UserManagementIndexMsg <| ShowHideDeleteUser Nothing
            }
    in
    modalView modalConfig


userCard : User -> Html Msgs.Msg
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
