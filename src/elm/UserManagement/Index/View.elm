module UserManagement.Index.View exposing (view)

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing
import UserManagement.Common.Models exposing (User)
import UserManagement.Index.Models exposing (..)
import UserManagement.Index.Msgs exposing (Msg(..))
import UserManagement.Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "user-management" ]
        [ pageHeader "User Management" indexActions
        , formSuccessResultView model.deletingUser
        , content wrapMsg model
        ]


content : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
content wrapMsg model =
    case model.users of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success users ->
            umTable wrapMsg model users


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.UserManagement Create) [ class "btn btn-primary" ] [ text "Create User" ] ]


umTable : (Msg -> Msgs.Msg) -> Model -> List User -> Html Msgs.Msg
umTable wrapMsg model users =
    table [ class "table" ]
        [ umTableHeader
        , umTableBody wrapMsg users
        , deleteModal wrapMsg model
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


umTableBody : (Msg -> Msgs.Msg) -> List User -> Html Msgs.Msg
umTableBody wrapMsg users =
    if List.isEmpty users then
        umTableEmpty
    else
        tbody [] (List.map (umTableRow wrapMsg) users)


umTableEmpty : Html msg
umTableEmpty =
    tr []
        [ td [ colspan 5, class "td-empty-table" ] [ text "There are no users." ] ]


umTableRow : (Msg -> Msgs.Msg) -> User -> Html Msgs.Msg
umTableRow wrapMsg user =
    tr []
        [ td [] [ text user.name ]
        , td [] [ text user.surname ]
        , td [] [ text user.email ]
        , td [] [ text user.role ]
        , td [ class "table-actions" ] [ umTableRowActionDelete wrapMsg user, umTableRowActionEdit user ]
        ]


umTableRowActionDelete : (Msg -> Msgs.Msg) -> User -> Html Msgs.Msg
umTableRowActionDelete wrapMsg user =
    a [ onClick <| wrapMsg <| ShowHideDeleteUser <| Just user ]
        [ i [ class "fa fa-trash-o" ] [] ]


umTableRowActionEdit : User -> Html Msgs.Msg
umTableRowActionEdit user =
    linkTo (Routing.UserManagement <| Edit user.uuid) [] [ i [ class "fa fa-edit" ] [] ]


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
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
            , actionMsg = wrapMsg DeleteUser
            , cancelMsg = wrapMsg <| ShowHideDeleteUser Nothing
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
