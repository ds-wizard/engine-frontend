module Common.Menu.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Auth.Msgs
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Html exposing (fa, linkToAttributes)
import Common.Html.Events exposing (onLinkClick)
import Common.Menu.Msgs exposing (Msg(ProfileMenuDropdownMsg, SetReportIssueOpen))
import Common.View exposing (modalView)
import Html exposing (Html, a, p, span, text)
import Html.Attributes exposing (class, href, target)
import Msgs
import Routing exposing (Route(Users))
import Users.Common.Models exposing (User)
import Users.Routing


viewProfileMenu : Maybe User -> Dropdown.State -> Html Msgs.Msg
viewProfileMenu maybeUser dropDownState =
    let
        ( name, initials ) =
            case maybeUser of
                Just user ->
                    ( user.name ++ " " ++ user.surname
                    , String.left 1 user.name ++ String.left 1 user.surname
                    )

                Nothing ->
                    ( "", "" )
    in
    Dropdown.dropdown dropDownState
        { options = [ Dropdown.dropRight ]
        , toggleMsg = Msgs.MenuMsg << ProfileMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ span [ class "full-name" ] [ text name, fa "angle-right" ]
                , span [ class "initials" ] [ text initials ]
                ]
        , items =
            [ Dropdown.anchorItem (linkToAttributes (Users <| Users.Routing.Edit "current"))
                [ fa "user-circle-o"
                , text "Edit profile"
                ]
            , Dropdown.anchorItem [ onLinkClick (Msgs.MenuMsg <| Common.Menu.Msgs.SetReportIssueOpen True) ]
                [ fa "exclamation-circle"
                , text "Report issue"
                ]
            , Dropdown.anchorItem [ onLinkClick (Msgs.AuthMsg Auth.Msgs.Logout) ]
                [ fa "sign-out"
                , text "Logout"
                ]
            ]
        }


viewReportIssueModal : Bool -> Html Msgs.Msg
viewReportIssueModal isOpen =
    let
        modalContent =
            [ p [] [ text "If you find any problem with the Wizard, the best way to report it, is to open an issue in our GitHub repository" ]
            , p []
                [ a [ class "link-with-icon", href "https://github.com/DataStewardshipWizard/dsw-common/issues", target "_blank" ]
                    [ fa "github"
                    , text "DataStewardshipWizard/dsw-common"
                    ]
                ]
            , p []
                [ text "You can also write us an email to "
                , a [ href "mailto:bugs@dsw.fairdata.solutions" ] [ text "bugs@dsw.fairdata.solutions" ]
                , text "."
                ]
            ]

        modalConfig =
            { modalTitle = "Report Issue"
            , modalContent = modalContent
            , visible = isOpen
            , actionResult = Unset
            , actionName = "Ok"
            , actionMsg = Msgs.MenuMsg <| SetReportIssueOpen False
            , cancelMsg = Nothing
            }
    in
    modalView modalConfig
