module Common.Menu.View exposing (..)

import Common.Html exposing (fa)
import Common.Menu.Msgs exposing (Msg(SetReportIssueOpen))
import Common.Types exposing (ActionResult(Unset))
import Common.View exposing (modalView)
import Html exposing (Html, a, p, text)
import Html.Attributes exposing (class, href, target)
import Msgs


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
            , cancelMsg = Msgs.MenuMsg <| SetReportIssueOpen False
            }
    in
    modalView modalConfig
