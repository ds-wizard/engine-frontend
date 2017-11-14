module UserManagement.Index.View exposing (..)

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import Routing exposing (Route(..))
import UserManagement.Index.Models exposing (..)
import UserManagement.Models exposing (User)


view : Model -> Html Msg
view model =
    div []
        [ pageHeader "User Management" indexActions
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
            umTable users


indexActions : List (Html Msg)
indexActions =
    [ linkTo UserManagementCreate [ class "btn btn-primary" ] [ text "Create User" ] ]


umTable : List User -> Html Msg
umTable users =
    table [ class "table" ]
        [ umTableHeader
        , umTableBody users
        ]


umTableHeader : Html Msg
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


umTableBody : List User -> Html Msg
umTableBody users =
    if List.isEmpty users then
        umTableEmpty
    else
        tbody [] (List.map umTableRow users)


umTableEmpty : Html msg
umTableEmpty =
    tr []
        [ td [ colspan 5, class "td-empty-table" ] [ text "There are no users." ] ]


umTableRow : User -> Html Msg
umTableRow user =
    tr []
        [ td [] [ text user.name ]
        , td [] [ text user.surname ]
        , td [] [ text user.email ]
        , td [] [ text user.role ]
        , td [ class "table-actions" ] [ umTableRowActionEdit user, umTableRowActionDelete user ]
        ]


umTableRowActionDelete : User -> Html Msg
umTableRowActionDelete user =
    linkTo (UserManagementDelete user.uuid) [] [ text "Delete" ]


umTableRowActionEdit : User -> Html Msg
umTableRowActionEdit user =
    linkTo (UserManagementEdit user.uuid) [] [ text "Edit" ]
