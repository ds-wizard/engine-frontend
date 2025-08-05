module Wizard.Common.View.FormActions exposing
    ( ViewDynamicConfig
    , view
    , viewCustomButton
    , viewDynamic
    , viewSubmit
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Shared.Components.FontAwesome exposing (faSpinner, faSuccess)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton


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


viewSubmit : AppState -> msg -> ActionButton.SubmitConfig a -> Html msg
viewSubmit appState cancelMsg submitConfig =
    div [ class "form-actions" ]
        [ button [ class "btn btn-secondary", onClick cancelMsg ] [ text (gettext "Cancel" appState.locale) ]
        , ActionButton.submit submitConfig
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
                [ class "btn btn-primary btn-with-loader"
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
