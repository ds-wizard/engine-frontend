module Common.View.Modal exposing
    ( AlertConfig
    , ConfirmConfig
    , alert
    , confirm
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import Common.View.ActionButton as ActionButton
import Common.View.Forms exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias ConfirmConfig msg =
    { modalTitle : String
    , modalContent : List (Html msg)
    , visible : Bool
    , actionResult : ActionResult String
    , actionName : String
    , actionMsg : msg
    , cancelMsg : Maybe msg
    }


confirm : ConfirmConfig msg -> Html msg
confirm cfg =
    let
        content =
            formResultView cfg.actionResult :: cfg.modalContent

        cancelDisabled =
            case cfg.actionResult of
                Loading ->
                    True

                _ ->
                    False

        cancelButton =
            case cfg.cancelMsg of
                Just cancelMsg ->
                    button [ onClick cancelMsg, disabled cancelDisabled, class "btn btn-secondary" ]
                        [ text "Cancel" ]

                Nothing ->
                    emptyNode
    in
    div [ class "modal-cover", classList [ ( "visible", cfg.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text cfg.modalTitle ]
                    ]
                , div [ class "modal-body" ]
                    content
                , div [ class "modal-footer" ]
                    [ ActionButton.button ( cfg.actionName, cfg.actionResult, cfg.actionMsg )
                    , cancelButton
                    ]
                ]
            ]
        ]


type alias AlertConfig msg =
    { message : String
    , visible : Bool
    , actionMsg : msg
    , actionName : String
    }


alert : AlertConfig msg -> Html msg
alert cfg =
    div [ class "modal-cover", classList [ ( "visible", cfg.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-body text-center" ]
                    [ p [] [ text cfg.message ]
                    , button [ onClick cfg.actionMsg, class "btn btn-primary" ] [ text cfg.actionName ]
                    ]
                ]
            ]
        ]
