module Users.Index.View exposing (view)

import Common.Html exposing (..)
import Common.View.FormResult as FormResult
import Common.View.Modal as Modal
import Common.View.Page as Page
import Common.View.Table as Table exposing (TableAction(..), TableActionLabel(..), TableConfig, TableFieldValue(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Routing
import Users.Common.Models exposing (User)
import Users.Index.Models exposing (..)
import Users.Index.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (viewUserList wrapMsg model) model.users


viewUserList : (Msg -> Msgs.Msg) -> Model -> List User -> Html Msgs.Msg
viewUserList wrapMsg model users =
    div [ class "col Users__Index" ]
        [ Page.header "Users" indexActions
        , FormResult.successOnlyView model.deletingUser
        , Page.actionResultView (Table.view tableConfig wrapMsg) model.users
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Users Create) [ class "btn btn-primary" ] [ text "Create User" ] ]


tableConfig : TableConfig User Msg
tableConfig =
    { emptyMessage = "There are no users."
    , fields =
        [ { label = "Name"
          , getValue = TextValue .name
          }
        , { label = "Surname"
          , getValue = TextValue .surname
          }
        , { label = "Email"
          , getValue = TextValue .email
          }
        , { label = "Role"
          , getValue = TextValue .role
          }
        , { label = "Active"
          , getValue = BoolValue .active
          }
        ]
    , actions =
        [ { label = TableActionDefault "edit" "Edit"
          , action = TableActionLink (Routing.Users << Edit << .uuid)
          , visible = always True
          }
        , { label = TableActionDestructive "trash-o" "Delete"
          , action = TableActionMsg tableActionDelete
          , visible = always True
          }
        ]
    , sortBy = .surname
    }


tableActionDelete : (Msg -> Msgs.Msg) -> User -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeleteUser << Just


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
                [ text "Are you sure you want to permanently delete the following user?" ]
            , userHtml
            ]

        modalConfig =
            { modalTitle = "Delete user"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingUser
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteUser
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteUser Nothing
            }
    in
    Modal.confirm modalConfig


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
