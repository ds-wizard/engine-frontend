module Common.View exposing (..)

import Common.View.Forms exposing (actionButton, errorView)
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
    div [ class "jumbotron full-page-error col-xs-12 col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2 col-lg-6 col-lg-offset-3" ]
        [ h1 [ class "display-3" ] [ i [ class ("fa " ++ icon) ] [] ]
        , p [] [ text error ]
        ]


type alias ModalConifg =
    { modalTitle : String
    , modalContent : List (Html Msg)
    , visible : Bool
    , actionActive : Bool
    , actionName : String
    , actionMsg : Msg
    , actionError : String
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
            errorView cfg.actionError :: cfg.modalContent
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
                    [ button [ onClick cfg.cancelMsg, disabled cfg.actionActive, class "btn btn-default" ]
                        [ text "Cancel" ]
                    , actionButton ( cfg.actionName, cfg.actionActive, cfg.actionMsg )
                    ]
                ]
            ]
        ]
