module Wizard.Common.View.Modal exposing
    ( ConfirmConfig
    , ConfirmExtraConfig
    , ErrorConfig
    , SimpleConfig
    , confirm
    , confirmExtra
    , error
    , simple
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, button, div, h5, pre, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult


type alias SimpleConfig msg =
    { modalContent : List (Html msg)
    , visible : Bool
    , dataCy : String
    }


simple : SimpleConfig msg -> Html msg
simple =
    simpleWithAttrs []


simpleWithAttrs : List (Attribute msg) -> SimpleConfig msg -> Html msg
simpleWithAttrs attributes cfg =
    div ([ class "modal modal-cover", classList [ ( "visible", cfg.visible ) ] ] ++ attributes)
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy ("modal_" ++ cfg.dataCy) ]
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
    , dataCy : String
    }


confirm : AppState -> ConfirmConfig msg -> Html msg
confirm appState cfg =
    confirmExtra appState
        { modalTitle = cfg.modalTitle
        , modalContent = cfg.modalContent
        , visible = cfg.visible
        , actionResult = cfg.actionResult
        , actionName = cfg.actionName
        , actionMsg = cfg.actionMsg
        , cancelMsg = cfg.cancelMsg
        , dangerous = cfg.dangerous
        , extraClass = ""
        , dataCy = cfg.dataCy
        }


type alias ConfirmExtraConfig msg =
    { modalTitle : String
    , modalContent : List (Html msg)
    , visible : Bool
    , actionResult : ActionResult String
    , actionName : String
    , actionMsg : msg
    , cancelMsg : Maybe msg
    , dangerous : Bool
    , extraClass : String
    , dataCy : String
    }


confirmExtra : AppState -> ConfirmExtraConfig msg -> Html msg
confirmExtra appState cfg =
    let
        content =
            FormResult.view appState cfg.actionResult :: cfg.modalContent

        cancelButton =
            case cfg.cancelMsg of
                Just cancelMsg ->
                    let
                        cancelDisabled =
                            ActionResult.isLoading cfg.actionResult
                    in
                    button
                        [ onClick cancelMsg
                        , disabled cancelDisabled
                        , class "btn btn-secondary"
                        , dataCy "modal_cancel-button"
                        ]
                        [ text (gettext "Cancel" appState.locale) ]

                Nothing ->
                    emptyNode
    in
    div [ class "modal modal-cover", class cfg.extraClass, classList [ ( "visible", cfg.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy ("modal_" ++ cfg.dataCy) ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text cfg.modalTitle ]
                    ]
                , div [ class "modal-body" ]
                    content
                , div [ class "modal-footer" ]
                    [ ActionButton.buttonWithAttrs appState <|
                        ActionButton.ButtonWithAttrsConfig cfg.actionName cfg.actionResult cfg.actionMsg cfg.dangerous [ dataCy "modal_action-button" ]
                    , cancelButton
                    ]
                ]
            ]
        ]


type alias ErrorConfig msg =
    { title : String
    , message : String
    , visible : Bool
    , actionMsg : msg
    , dataCy : String
    }


error : AppState -> ErrorConfig msg -> Html msg
error appState cfg =
    let
        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text cfg.title ] ]
            , div [ class "modal-body" ]
                [ pre [ class "pre-error" ] [ text cfg.message ]
                ]
            , div [ class "modal-footer" ]
                [ button
                    [ onClick cfg.actionMsg
                    , class "btn btn-primary"
                    ]
                    [ text (gettext "OK" appState.locale) ]
                ]
            ]

        modalConfig =
            { modalContent = modalContent
            , visible = cfg.visible
            , dataCy = cfg.dataCy
            }
    in
    simpleWithAttrs [ class "modal-error" ] modalConfig
