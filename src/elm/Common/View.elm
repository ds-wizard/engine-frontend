module Common.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View.Forms exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)


pageHeader : String -> List (Html msg) -> Html msg
pageHeader title actions =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , pageActions actions
        ]


pageActions : List (Html msg) -> Html msg
pageActions actions =
    div [ class "actions" ]
        actions


fullPageLoader : Html msg
fullPageLoader =
    div [ class "full-page-loader" ]
        [ i [ class "fa fa-spinner fa-spin" ] []
        , div [] [ text "Loading..." ]
        ]


defaultFullPageError : String -> Html msg
defaultFullPageError =
    fullPageError "fa-frown-o"


fullPageError : String -> String -> Html msg
fullPageError icon error =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class ("fa " ++ icon) ] [] ]
        , p [] [ text error ]
        ]


fullPageActionResultView : (a -> Html Msgs.Msg) -> ActionResult a -> Html Msgs.Msg
fullPageActionResultView viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success result ->
            viewContent result


type alias ModalConifg =
    { modalTitle : String
    , modalContent : List (Html Msg)
    , visible : Bool
    , actionResult : ActionResult String
    , actionName : String
    , actionMsg : Msg
    , cancelMsg : Msg
    }


modalView : ModalConifg -> Html Msg
modalView cfg =
    let
        visibleClass =
            if cfg.visible then
                "visible"
            else
                ""

        content =
            formResultView cfg.actionResult :: cfg.modalContent

        cancelDisabled =
            case cfg.actionResult of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class ("modal-cover " ++ visibleClass) ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text cfg.modalTitle ]
                    ]
                , div [ class "modal-body" ]
                    content
                , div [ class "modal-footer" ]
                    [ button [ onClick cfg.cancelMsg, disabled cancelDisabled, class "btn btn-default" ]
                        [ text "Cancel" ]
                    , actionButton ( cfg.actionName, cfg.actionResult, cfg.actionMsg )
                    ]
                ]
            ]
        ]
