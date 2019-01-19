module Common.Menu.View exposing
    ( viewAboutModal
    , viewAboutModalContent
    , viewBuildInfo
    , viewHelpMenu
    , viewProfileMenu
    , viewReportIssueModal
    )

import ActionResult exposing (ActionResult(..))
import Auth.Msgs
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Html exposing (fa, linkToAttributes)
import Common.Html.Events exposing (onLinkClick)
import Common.Menu.Models exposing (BuildInfo, clientBuildInfo)
import Common.Menu.Msgs exposing (Msg(..))
import Common.View exposing (fullPageActionResultView, modalView)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href, target)
import Msgs
import Routing exposing (Route(..))
import Users.Common.Models exposing (User)
import Users.Routing


viewHelpMenu : Dropdown.State -> Html Msgs.Msg
viewHelpMenu dropdownState =
    Dropdown.dropdown dropdownState
        { options = [ Dropdown.dropRight ]
        , toggleMsg = Msgs.MenuMsg << HelpMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ fa "question-circle"
                , span [ class "sidebar-link" ] [ text "Help", fa "angle-right" ]
                ]
        , items =
            [ Dropdown.anchorItem [ onLinkClick (Msgs.MenuMsg <| Common.Menu.Msgs.SetAboutOpen True) ]
                [ fa "info"
                , text "About"
                ]
            , Dropdown.anchorItem [ onLinkClick (Msgs.MenuMsg <| Common.Menu.Msgs.SetReportIssueOpen True) ]
                [ fa "exclamation-triangle"
                , text "Report issue"
                ]
            ]
        }


viewProfileMenu : Maybe User -> Dropdown.State -> Html Msgs.Msg
viewProfileMenu maybeUser dropdownState =
    let
        name =
            case maybeUser of
                Just user ->
                    user.name ++ " " ++ user.surname

                Nothing ->
                    ""
    in
    Dropdown.dropdown dropdownState
        { options = [ Dropdown.dropRight ]
        , toggleMsg = Msgs.MenuMsg << ProfileMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ fa "user-circle"
                , span [ class "sidebar-link" ] [ text name, fa "angle-right" ]
                ]
        , items =
            [ Dropdown.anchorItem (linkToAttributes (Users <| Users.Routing.Edit "current"))
                [ fa "user"
                , text "Edit profile"
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
            [ p [] [ text "If you find any problem with the Wizard, the best way to report it is to open an issue in our GitHub repository" ]
            , p []
                [ a [ class "link-with-icon", href "https://github.com/ds-wizard/dsw-common/issues", target "_blank" ]
                    [ fa "github"
                    , text "ds-wizard/dsw-common"
                    ]
                ]
            , p []
                [ text "You can also write us an email to "
                , a [ href "mailto:bugs@ds-wizard.org" ] [ text "bugs@ds-wizard.org" ]
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


viewAboutModal : Bool -> ActionResult BuildInfo -> Html Msgs.Msg
viewAboutModal isOpen serverBuildInfoActionResult =
    let
        modalContent =
            fullPageActionResultView viewAboutModalContent serverBuildInfoActionResult

        modalConfig =
            { modalTitle = "About"
            , modalContent = [ modalContent ]
            , visible = isOpen
            , actionResult = Unset
            , actionName = "Ok"
            , actionMsg = Msgs.MenuMsg <| SetAboutOpen False
            , cancelMsg = Nothing
            }
    in
    modalView modalConfig


viewAboutModalContent : BuildInfo -> Html Msgs.Msg
viewAboutModalContent serverBuildInfo =
    div []
        [ viewBuildInfo "Client" clientBuildInfo
        , viewBuildInfo "Server" serverBuildInfo
        ]


viewBuildInfo : String -> BuildInfo -> Html Msgs.Msg
viewBuildInfo name buildInfo =
    table [ class "table table-borderless table-build-info" ]
        [ thead []
            [ tr []
                [ th [ colspan 2 ] [ text name ] ]
            ]
        , tbody []
            [ tr []
                [ td [] [ text "Version" ]
                , td [] [ code [] [ text buildInfo.version ] ]
                ]
            , tr []
                [ td [] [ text "Built at" ]
                , td [] [ em [] [ text buildInfo.builtAt ] ]
                ]
            ]
        ]
