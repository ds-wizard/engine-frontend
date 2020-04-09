module Wizard.Common.View.Modal exposing
    ( AlertConfig
    , ConfirmConfig
    , SimpleConfig
    , alert
    , confirm
    , simple
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.Modal"


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


confirm : AppState -> ConfirmConfig msg -> Html msg
confirm appState cfg =
    let
        content =
            FormResult.view appState cfg.actionResult :: cfg.modalContent

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
                        [ lx_ "button.cancel" appState ]

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
                    [ ActionButton.button appState <| ActionButton.ButtonConfig cfg.actionName cfg.actionResult cfg.actionMsg cfg.dangerous
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
