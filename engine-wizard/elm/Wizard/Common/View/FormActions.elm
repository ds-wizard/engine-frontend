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
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Routes as Routes


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
view : AppState -> Routes.Route -> ActionButton.ButtonConfig a msg -> Html msg
view appState cancelRoute actionButtonConfig =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ text (gettext "Cancel" appState.locale) ]
        , ActionButton.button appState actionButtonConfig
        ]


viewCustomButton : AppState -> Routes.Route -> Html msg -> Html msg
viewCustomButton appState cancelRoute actionButton =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ text (gettext "Cancel" appState.locale) ]
        , actionButton
        ]


viewSubmit : AppState -> Routes.Route -> ActionButton.SubmitConfig a -> Html msg
viewSubmit appState cancelRoute submitConfig =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ text (gettext "Cancel" appState.locale) ]
        , ActionButton.submit appState submitConfig
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
                    faSet "_global.spinner" appState

                ActionResult.Success _ ->
                    faSet "_global.success" appState

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
