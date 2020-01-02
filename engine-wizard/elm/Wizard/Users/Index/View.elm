module Wizard.Users.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (..)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Users.Common.User exposing (User)
import Wizard.Users.Index.Models exposing (..)
import Wizard.Users.Index.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Users.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Users.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewUserList appState model) model.users


viewUserList : AppState -> Model -> List User -> Html Msg
viewUserList appState model users =
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
        [ Page.header (lg "users" appState) (indexActions appState)
        , FormResult.successOnlyView appState model.deletingUser
        , Listing.view appState (listingConfig appState) <| List.sortWith sortUsers users
        , deleteModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    [ linkTo appState
        (Routes.UsersRoute CreateRoute)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]
    ]


listingConfig : AppState -> ListingConfig User Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription
    , actions = listingActions appState
    , textTitle = \u -> u.surname ++ u.name
    , emptyText = l_ "listing.empty" appState
    , updated = Nothing
    }


listingTitle : AppState -> User -> Html Msg
listingTitle appState user =
    span []
        [ linkTo appState (detailRoute user) [] [ text <| user.name ++ " " ++ user.surname ]
        , listingTitleBadge appState user
        ]


listingTitleBadge : AppState -> User -> Html msg
listingTitleBadge appState user =
    let
        activeBadge =
            if user.active then
                emptyNode

            else
                span [ class "badge badge-danger" ]
                    [ lx_ "badge.inactive" appState ]
    in
    span []
        [ span [ class "badge badge-light" ]
            [ text user.role ]
        , activeBadge
        ]


listingDescription : User -> Html Msg
listingDescription user =
    span []
        [ a [ class "fragment", href <| "mailto:" ++ user.email ]
            [ text user.email ]
        ]


listingActions : AppState -> User -> List (ListingActionConfig Msg)
listingActions appState user =
    [ { extraClass = Nothing
      , icon = Just <| faSet "_global.edit" appState
      , label = l_ "action.edit" appState
      , msg = ListingActionLink (detailRoute user)
      }
    , { extraClass = Just "text-danger"
      , icon = Just <| faSet "_global.delete" appState
      , label = l_ "action.delete" appState
      , msg = ListingActionMsg (ShowHideDeleteUser <| Just user)
      }
    ]


detailRoute : User -> Routes.Route
detailRoute =
    Routes.UsersRoute << EditRoute << .uuid


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, userHtml ) =
            case model.userToBeDeleted of
                Just user ->
                    ( True, userCard appState user )

                Nothing ->
                    ( False, emptyNode )

        modalContent =
            [ p []
                [ lx_ "deleteModal.message" appState ]
            , userHtml
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingUser
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteUser
            , cancelMsg = Just <| ShowHideDeleteUser Nothing
            , dangerous = True
            }
    in
    Modal.confirm appState modalConfig


userCard : AppState -> User -> Html Msg
userCard appState user =
    div [ class "user-card" ]
        [ div [ class "icon" ] [ faSet "userCard.icon" appState ]
        , div [ class "name" ] [ text (user.name ++ " " ++ user.surname) ]
        , div [ class "email" ]
            [ a [ href ("mailto:" ++ user.email) ] [ text user.email ]
            ]
        , div [ class "role" ]
            [ text (lg "user.role" appState ++ ": " ++ user.role)
            ]
        ]
