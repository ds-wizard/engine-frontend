module Wizard.Common.Menu.View exposing
    ( viewAboutModal
    , viewAboutModalContent
    , viewBuildInfo
    , viewHelpMenu
    , viewProfileMenu
    , viewReportIssueModal
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href, target)
import Shared.Locale exposing (l, lh, lx)
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet)
import Wizard.Common.Html.Attribute exposing (linkToAttributes)
import Wizard.Common.Html.Events exposing (onLinkClick)
import Wizard.Common.Menu.Models exposing (BuildInfo, clientBuildInfo)
import Wizard.Common.Menu.Msgs exposing (Msg(..))
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Users.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Menu.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Menu.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Menu.View"


viewHelpMenu : AppState -> Dropdown.State -> Html Wizard.Msgs.Msg
viewHelpMenu appState dropdownState =
    Dropdown.dropdown dropdownState
        { options = [ Dropdown.dropRight ]
        , toggleMsg = Wizard.Msgs.MenuMsg << HelpMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ faSet "menu.help" appState
                , span [ class "sidebar-link" ]
                    [ span [] [ lx_ "helpMenu.help" appState ], faSet "menu.dropdownToggle" appState ]
                ]
        , items =
            [ Dropdown.anchorItem [ onLinkClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetAboutOpen True) ]
                [ faSet "menu.about" appState
                , lx_ "helpMenu.about" appState
                ]
            , Dropdown.anchorItem [ onLinkClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetReportIssueOpen True) ]
                [ faSet "menu.reportIssue" appState
                , lx_ "helpMenu.reportIssue" appState
                ]
            ]
        }


viewProfileMenu : AppState -> Dropdown.State -> Html Wizard.Msgs.Msg
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
        , toggleMsg = Wizard.Msgs.MenuMsg << ProfileMenuDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ faSet "menu.user" appState
                , span [ class "sidebar-link" ] [ span [] [ text name ], faSet "menu.dropdownToggle" appState ]
                ]
        , items =
            [ Dropdown.anchorItem (linkToAttributes appState (Routes.UsersRoute <| Wizard.Users.Routes.EditRoute "current"))
                [ faSet "menu.profile" appState
                , lx_ "profileMenu.edit" appState
                ]
            , Dropdown.anchorItem [ onLinkClick (Wizard.Msgs.AuthMsg Wizard.Auth.Msgs.Logout) ]
                [ faSet "menu.logout" appState
                , lx_ "profileMenu.logout" appState
                ]
            ]
        }


viewReportIssueModal : AppState -> Bool -> Html Wizard.Msgs.Msg
viewReportIssueModal appState isOpen =
    let
        supportMailLink =
            a [ href <| "mailto:" ++ appState.config.client.supportEmail ]
                [ text appState.config.client.supportEmail ]

        modalContent =
            [ p [] [ lx_ "reportModal.info" appState ]
            , p []
                [ a [ class "link-with-icon", href appState.config.client.supportRepositoryUrl, target "_blank" ]
                    [ faSet "report.repository" appState
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
            , actionMsg = Wizard.Msgs.MenuMsg <| SetReportIssueOpen False
            , cancelMsg = Nothing
            , dangerous = False
            }
    in
    Modal.confirm appState modalConfig


viewAboutModal : AppState -> Bool -> ActionResult BuildInfo -> Html Wizard.Msgs.Msg
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
            , actionMsg = Wizard.Msgs.MenuMsg <| SetAboutOpen False
            , cancelMsg = Nothing
            , dangerous = False
            }
    in
    Modal.confirm appState modalConfig


viewAboutModalContent : AppState -> BuildInfo -> Html Wizard.Msgs.Msg
viewAboutModalContent appState serverBuildInfo =
    div []
        [ viewBuildInfo appState (l_ "about.client" appState) clientBuildInfo
        , viewBuildInfo appState (l_ "about.server" appState) serverBuildInfo
        ]


viewBuildInfo : AppState -> String -> BuildInfo -> Html Wizard.Msgs.Msg
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
