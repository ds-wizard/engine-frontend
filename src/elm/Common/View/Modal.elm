module Common.View.Modal exposing
    ( AlertConfig
    , ConfirmConfig
    , SimpleConfig
    , alert
    , confirm
    , simple
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias SimpleConfig msg =
    { modalContent : List (Html msg)
    , visible : Bool
    }


simple : SimpleConfig msg -> Html msg
simple cfg =
    div [ class "modal-cover", classList [ ( "visible", cfg.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                cfg.modalContent
            ]
        ]


type alias ConfirmConfig msg =
    { modalTitle : String
    , modalContent : List (Html msg)
    , visible : Bool
    , actionResult : ActionResult String
    , actionName : String
    , actionMsg : msg
    , cancelMsg : Maybe msg
    , dangerous : Bool
    }


confirm : ConfirmConfig msg -> Html msg
confirm cfg =
    let
        content =
            FormResult.view cfg.actionResult :: cfg.modalContent

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
                    [ ActionButton.button <| ActionButton.ButtonConfig cfg.actionName cfg.actionResult cfg.actionMsg cfg.dangerous
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
