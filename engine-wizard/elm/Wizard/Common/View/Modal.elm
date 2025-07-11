module Wizard.Common.View.Modal exposing
    ( ConfirmConfig
    , ErrorConfig
    , SimpleConfig
    , confirm
    , confirmConfig
    , confirmConfigAction
    , confirmConfigActionResult
    , confirmConfigCancelMsg
    , confirmConfigCancelShortcutMsg
    , confirmConfigContent
    , confirmConfigDangerous
    , confirmConfigDataCy
    , confirmConfigExtraClass
    , confirmConfigGuideLink
    , confirmConfigMbCancelMsg
    , confirmConfigVisible
    , error
    , simple
    , simpleWithAttrs
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, button, div, h5, pre, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Shortcut
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks exposing (GuideLinks)
import Wizard.Common.Html exposing (guideLink)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult


type alias SimpleConfig msg =
    { modalContent : List (Html msg)
    , visible : Bool
    , enterMsg : Maybe msg
    , escMsg : Maybe msg
    , dataCy : String
    }


simple : SimpleConfig msg -> Html msg
simple =
    simpleWithAttrs []


simpleWithAttrs : List (Attribute msg) -> SimpleConfig msg -> Html msg
simpleWithAttrs attributes cfg =
    let
        shortcuts =
            if cfg.visible then
                Maybe.values
                    [ Maybe.map (Shortcut.simpleShortcut Shortcut.Enter) cfg.enterMsg
                    , Maybe.map (Shortcut.simpleShortcut Shortcut.Escape) cfg.escMsg
                    ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts
        ([ class "modal modal-cover", classList [ ( "visible", cfg.visible ) ] ] ++ attributes)
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy ("modal_" ++ cfg.dataCy) ]
                cfg.modalContent
            ]
        ]


type ConfirmConfig msg
    = ConfirmConfig (ConfirmConfigData msg)


type alias ConfirmConfigData msg =
    { modalTitle : String
    , modalContent : List (Html msg)
    , visible : Bool
    , actionResult : ActionResult String
    , action : Maybe ( String, msg )
    , cancelMsg : Maybe msg
    , cancelShortcutMsg : Maybe msg
    , dangerous : Bool
    , extraClass : Maybe String
    , guideLink : Maybe (GuideLinks -> String)
    , dataCy : Maybe String
    }


confirmConfig : String -> ConfirmConfig msg
confirmConfig title =
    ConfirmConfig
        { modalTitle = title
        , modalContent = []
        , visible = False
        , actionResult = ActionResult.Unset
        , action = Nothing
        , cancelMsg = Nothing
        , cancelShortcutMsg = Nothing
        , dangerous = False
        , extraClass = Nothing
        , guideLink = Nothing
        , dataCy = Nothing
        }


confirmConfigContent : List (Html msg) -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigContent content (ConfirmConfig data) =
    ConfirmConfig { data | modalContent = content }


confirmConfigVisible : Bool -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigVisible visible (ConfirmConfig data) =
    ConfirmConfig { data | visible = visible }


confirmConfigActionResult : ActionResult String -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigActionResult actionResult (ConfirmConfig data) =
    ConfirmConfig { data | actionResult = actionResult }


confirmConfigAction : String -> msg -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigAction actionName actionMsg (ConfirmConfig data) =
    ConfirmConfig { data | action = Just ( actionName, actionMsg ) }


confirmConfigCancelMsg : msg -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigCancelMsg cancelMsg (ConfirmConfig data) =
    ConfirmConfig { data | cancelMsg = Just cancelMsg }


confirmConfigMbCancelMsg : Maybe msg -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigMbCancelMsg cancelMsg (ConfirmConfig data) =
    ConfirmConfig { data | cancelMsg = cancelMsg }


confirmConfigCancelShortcutMsg : msg -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigCancelShortcutMsg cancelShortcutMsg (ConfirmConfig data) =
    ConfirmConfig { data | cancelShortcutMsg = Just cancelShortcutMsg }


confirmConfigDangerous : Bool -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigDangerous dangerous (ConfirmConfig data) =
    ConfirmConfig { data | dangerous = dangerous }


confirmConfigExtraClass : String -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigExtraClass extraClass (ConfirmConfig data) =
    ConfirmConfig { data | extraClass = Just extraClass }


confirmConfigGuideLink : (GuideLinks -> String) -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigGuideLink guideLink (ConfirmConfig data) =
    ConfirmConfig { data | guideLink = Just guideLink }


confirmConfigDataCy : String -> ConfirmConfig msg -> ConfirmConfig msg
confirmConfigDataCy dataCy (ConfirmConfig data) =
    ConfirmConfig { data | dataCy = Just dataCy }


confirm : AppState -> ConfirmConfig msg -> Html msg
confirm appState (ConfirmConfig data) =
    let
        content =
            FormResult.view data.actionResult :: data.modalContent

        actionsDisabled =
            not data.visible || ActionResult.isLoading data.actionResult

        wrapShortcut shortcut =
            if actionsDisabled then
                Nothing

            else
                Just shortcut

        ( actionButton, actionShortcut ) =
            case data.action of
                Just ( actionName, actionMsg ) ->
                    let
                        btn =
                            ActionButton.buttonWithAttrs <|
                                ActionButton.ButtonWithAttrsConfig actionName data.actionResult actionMsg data.dangerous [ dataCy "modal_action-button" ]

                        shortcut =
                            wrapShortcut (Shortcut.simpleShortcut Shortcut.Enter actionMsg)
                    in
                    ( btn, shortcut )

                Nothing ->
                    ( Html.nothing, Nothing )

        ( cancelButton, cancelShortcut ) =
            case data.cancelMsg of
                Just cancelMsg ->
                    let
                        btn =
                            button
                                [ onClick cancelMsg
                                , disabled actionsDisabled
                                , class "btn btn-secondary"
                                , dataCy "modal_cancel-button"
                                ]
                                [ text (gettext "Cancel" appState.locale) ]

                        shortcut =
                            wrapShortcut (Shortcut.simpleShortcut Shortcut.Escape cancelMsg)
                    in
                    ( btn, shortcut )

                Nothing ->
                    case data.cancelShortcutMsg of
                        Just cancelShortcutMsg ->
                            let
                                shortcut =
                                    wrapShortcut (Shortcut.simpleShortcut Shortcut.Escape cancelShortcutMsg)
                            in
                            ( Html.nothing, shortcut )

                        Nothing ->
                            ( Html.nothing, Nothing )

        mbGuideLink =
            case data.guideLink of
                Just link ->
                    guideLink appState link

                Nothing ->
                    Html.nothing

        shortcuts =
            Maybe.values [ actionShortcut, cancelShortcut ]
    in
    Shortcut.shortcutElement shortcuts
        [ class "modal modal-cover", class (Maybe.withDefault "" data.extraClass), classList [ ( "visible", data.visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy ("modal_" ++ Maybe.withDefault "confirm" data.dataCy) ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text data.modalTitle ]
                    , mbGuideLink
                    ]
                , div [ class "modal-body" ]
                    content
                , div [ class "modal-footer" ]
                    [ actionButton, cancelButton ]
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
            , enterMsg = Just cfg.actionMsg
            , escMsg = Just cfg.actionMsg
            , dataCy = cfg.dataCy
            }
    in
    simpleWithAttrs [ class "modal-error" ] modalConfig
