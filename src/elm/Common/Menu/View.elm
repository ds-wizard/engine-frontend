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
import Common.AppState exposing (AppState)
import Common.Html exposing (fa, faSet)
import Common.Html.Attribute exposing (linkToAttributes)
import Common.Html.Events exposing (onLinkClick)
import Common.Locale exposing (l, lh, lx)
import Common.Menu.Models exposing (BuildInfo, clientBuildInfo)
import Common.Menu.Msgs exposing (Msg(..))
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href, target)
import Msgs
import Routes
import Users.Routes
import Users.Routing


l_ : String -> AppState -> String
l_ =
    l "Common.Menu.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Common.Menu.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Common.Menu.View"


viewHelpMenu : AppState -> Dropdown.State -> Html Msgs.Msg
viewHelpMenu appState dropdownState =
    Dropdown.dropdown dropdownState
        { options = [ Dropdown.dropRight ]
        , toggleMsg = Msgs.MenuMsg << HelpMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ faSet "menu.help" appState
                , span [ class "sidebar-link" ]
                    [ lx_ "helpMenu.help" appState, fa "angle-right" ]
                ]
        , items =
            [ Dropdown.anchorItem [ onLinkClick (Msgs.MenuMsg <| Common.Menu.Msgs.SetAboutOpen True) ]
                [ faSet "menu.about" appState
                , lx_ "helpMenu.about" appState
                ]
            , Dropdown.anchorItem [ onLinkClick (Msgs.MenuMsg <| Common.Menu.Msgs.SetReportIssueOpen True) ]
                [ faSet "menu.reportIssue" appState
                , lx_ "helpMenu.reportIssue" appState
                ]
            ]
        }


viewProfileMenu : AppState -> Dropdown.State -> Html Msgs.Msg
viewProfileMenu appState dropdownState =
    let
        name =
            case appState.session.user of
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
                [ faSet "menu.user" appState
                , span [ class "sidebar-link" ] [ text name, fa "angle-right" ]
                ]
        , items =
            [ Dropdown.anchorItem (linkToAttributes appState (Routes.UsersRoute <| Users.Routes.EditRoute "current"))
                [ faSet "menu.profile" appState
                , lx_ "profileMenu.edit" appState
                ]
            , Dropdown.anchorItem [ onLinkClick (Msgs.AuthMsg Auth.Msgs.Logout) ]
                [ faSet "menu.logout" appState
                , lx_ "profileMenu.logout" appState
                ]
            ]
        }


viewReportIssueModal : AppState -> Bool -> Html Msgs.Msg
viewReportIssueModal appState isOpen =
    let
        supportMailLink =
            a [ href <| "mailto:" ++ appState.config.client.supportEmail ]
                [ text appState.config.client.supportEmail ]

        modalContent =
            [ p [] [ lx_ "reportModal.info" appState ]
            , p []
                [ a [ class "link-with-icon", href appState.config.client.supportRepositoryUrl, target "_blank" ]
                    [ faSet "reportIssue.repository" appState
                    , text appState.config.client.supportRepositoryName
                    ]
                ]
            , p [] (lh_ "reportModal.writeUs" [ supportMailLink ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "reportModal.title" appState
            , modalContent = modalContent
            , visible = isOpen
            , actionResult = Unset
            , actionName = l_ "reportModal.action" appState
            , actionMsg = Msgs.MenuMsg <| SetReportIssueOpen False
            , cancelMsg = Nothing
            , dangerous = False
            }
    in
    Modal.confirm modalConfig


viewAboutModal : AppState -> Bool -> ActionResult BuildInfo -> Html Msgs.Msg
viewAboutModal appState isOpen serverBuildInfoActionResult =
    let
        modalContent =
            Page.actionResultView appState (viewAboutModalContent appState) serverBuildInfoActionResult

        modalConfig =
            { modalTitle = l_ "about.title" appState
            , modalContent = [ modalContent ]
            , visible = isOpen
            , actionResult = Unset
            , actionName = l_ "about.action" appState
            , actionMsg = Msgs.MenuMsg <| SetAboutOpen False
            , cancelMsg = Nothing
            , dangerous = False
            }
    in
    Modal.confirm modalConfig


viewAboutModalContent : AppState -> BuildInfo -> Html Msgs.Msg
viewAboutModalContent appState serverBuildInfo =
    div []
        [ viewBuildInfo appState (l_ "about.client" appState) clientBuildInfo
        , viewBuildInfo appState (l_ "about.server" appState) serverBuildInfo
        ]


viewBuildInfo : AppState -> String -> BuildInfo -> Html Msgs.Msg
viewBuildInfo appState name buildInfo =
    table [ class "table table-borderless table-build-info" ]
        [ thead []
            [ tr []
                [ th [ colspan 2 ] [ text name ] ]
            ]
        , tbody []
            [ tr []
                [ td [] [ lx_ "about.version" appState ]
                , td [] [ code [] [ text buildInfo.version ] ]
                ]
            , tr []
                [ td [] [ lx_ "about.builtAt" appState ]
                , td [] [ em [] [ text buildInfo.builtAt ] ]
                ]
            ]
        ]
