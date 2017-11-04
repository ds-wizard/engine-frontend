module UserManagement.Index.View exposing (..)

import Common.Html exposing (defaultFullPageError, fullPageLoader, linkTo, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import Routing exposing (Route(..))
import UserManagement.Index.Models exposing (..)
import UserManagement.Models exposing (User)


view : Model -> Html Msg
view model =
    let
        content =
            if model.loading then
                fullPageLoader
            else if model.error /= "" then
                defaultFullPageError model.error
            else
                umTable model
    in
    div []
        [ pageHeader "User Management" indexActions
        , content
        ]


indexActions : List (Html Msg)
indexActions =
    [ linkTo UserManagementCreate [ class "btn btn-primary" ] [ text "Create User" ] ]


umTable : Model -> Html Msg
umTable model =
    table [ class "table" ]
        [ umTableHeader
        , umTableBody model
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


umTableBody : Model -> Html Msg
umTableBody model =
    tbody [] (List.map umTableRow model.users)


umTableRow : User -> Html Msg
umTableRow user =
    tr []
        [ td [] [ text user.name ]
        , td [] [ text user.surname ]
        , td [] [ text user.email ]
        , td [] [ text user.role ]
        , td [ class "table-actions" ] [ umTableRowAction "Edit", umTableRowActionDelete user ]
        ]


umTableRowActionDelete : User -> Html Msg
umTableRowActionDelete user =
    linkTo (UserManagementDelete user.uuid) [] [ text "Delete" ]


umTableRowAction : String -> Html Msg
umTableRowAction name =
    a [ href "#" ]
        [ text name ]
