module Users.Index.View exposing (view)

import Common.Html exposing (..)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
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
    let
        sortUsers u1 u2 =
            case compare (String.toLower u1.surname) (String.toLower u2.surname) of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compare (String.toLower u1.name) (String.toLower u2.name)
    in
    div [ listClass "Users__Index" ]
        [ Page.header "Users" indexActions
        , FormResult.successOnlyView model.deletingUser
        , Listing.view (listingConfig wrapMsg) <| List.sortWith sortUsers users
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Users Create) [ class "btn btn-primary" ] [ text "Create" ] ]


listingConfig : (Msg -> Msgs.Msg) -> ListingConfig User Msgs.Msg
listingConfig wrapMsg =
    { title = listingTitle
    , description = listingDescription
    , actions = listingActions wrapMsg
    , textTitle = \u -> u.surname ++ u.name
    , emptyText = "Click \"Create\" button to add a new User."
    }


listingTitle : User -> Html Msgs.Msg
listingTitle user =
    span []
        [ linkTo (detailRoute user) [] [ text <| user.name ++ " " ++ user.surname ]
        , listingTitleBadge user
        ]


listingTitleBadge : User -> Html msg
listingTitleBadge user =
    let
        activeBadge =
            if user.active then
                emptyNode

            else
                span [ class "badge badge-danger" ]
                    [ text "inactive" ]
    in
    span []
        [ span [ class "badge badge-light" ]
            [ text user.role ]
        , activeBadge
        ]


listingDescription : User -> Html Msgs.Msg
listingDescription user =
    span []
        [ a [ class "fragment", href <| "mailto:" ++ user.email ]
            [ text user.email ]
        ]


listingActions : (Msg -> Msgs.Msg) -> User -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg user =
    [ { extraClass = Nothing
      , icon = Just "edit"
      , label = "Edit"
      , msg = ListingActionLink (detailRoute user)
      }
    , { extraClass = Just "text-danger"
      , icon = Just "trash-o"
      , label = "Delete"
      , msg = ListingActionMsg (wrapMsg <| ShowHideDeleteUser <| Just user)
      }
    ]


detailRoute : User -> Routing.Route
detailRoute =
    Routing.Users << Edit << .uuid


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
            , dangerous = True
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
