module Wizard.Components.FormActions exposing
    ( ViewDynamicConfig
    , view
    , viewCustomButton
    , viewDynamic
    )

import ActionResult exposing (ActionResult)
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (faSpinner, faSuccess)
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, disabled, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Wizard.Data.AppState exposing (AppState)


view : AppState -> msg -> ActionButton.ButtonConfig a msg -> Html msg
view appState cancelMsg actionButtonConfig =
    div [ class "form-actions" ]
        [ button [ class "btn btn-secondary", onClick cancelMsg ] [ text (gettext "Cancel" appState.locale) ]
        , ActionButton.button actionButtonConfig
        ]


viewCustomButton : AppState -> msg -> Html msg -> Html msg
viewCustomButton appState cancelMsg actionButton =
    div [ class "form-actions" ]
        [ button [ class "btn btn-secondary", onClick cancelMsg ] [ text (gettext "Cancel" appState.locale) ]
        , actionButton
        ]


type alias ViewDynamicConfig a =
    { text : Maybe String
    , actionResult : ActionResult a
    , formChanged : Bool
    , wide : Bool
    }


viewDynamic : ViewDynamicConfig a -> AppState -> Html msg
viewDynamic cfg appState =
    let
        isVisible =
            cfg.formChanged || ActionResult.isLoading cfg.actionResult

        isDisabled =
            ActionResult.isLoading cfg.actionResult || ActionResult.isSuccess cfg.actionResult

        formActionsText =
            Maybe.withDefault
                (gettext "You have unsaved changes." appState.locale)
                cfg.text

        content =
            case cfg.actionResult of
                ActionResult.Loading ->
                    faSpinner

                ActionResult.Success _ ->
                    faSuccess

                _ ->
                    text (gettext "Save" appState.locale)

        saveButton =
            button
                [ class "btn btn-primary btn-wide"
                , disabled isDisabled
                , type_ "submit"
                , dataCy "form_submit"
                ]
                [ content ]
    in
    div
        [ class "form-actions-dynamic"
        , classList
            [ ( "form-actions-dynamic-visible", isVisible )
            , ( "form-actions-dynamic-wide", cfg.wide )
            ]
        ]
        [ p [] [ text formActionsText ]
        , saveButton
        ]
