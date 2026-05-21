module Wizard.Pages.Settings.Common.AuthButtonFormGroup exposing (formGroup)

import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class, placeholder)
import String.Extra as String
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Data.AppState exposing (AppState)


formGroup : AppState -> Form FormError a -> Html Form.Msg
formGroup appState form =
    let
        buttonName =
            (Form.getFieldAsString "name" form).value
                |> Maybe.withDefault ""

        buttonIcon =
            (Form.getFieldAsString "styleIcon" form).value
                |> Maybe.andThen String.toMaybe

        buttonColor =
            (Form.getFieldAsString "styleColor" form).value
                |> Maybe.andThen String.toMaybe

        buttonBackground =
            (Form.getFieldAsString "styleBackground" form).value
                |> Maybe.andThen String.toMaybe
    in
    div [ class "row" ]
        [ div [ class "col-7" ]
            [ div [ class "row mb-3" ]
                [ div [ class "col" ]
                    [ FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultIcon ] appState.locale form "styleIcon" (gettext "Icon" appState.locale)
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultBackground ] appState.locale form "styleBackground" (gettext "Background Color" appState.locale)
                    ]
                , div [ class "col" ]
                    [ FormGroup.inputAttrs [ placeholder ExternalLoginButton.defaultColor ] appState.locale form "styleColor" (gettext "Text Color" appState.locale)
                    ]
                ]
            ]
        , div [ class "col-4 offset-1" ]
            [ div [ class "form-group" ]
                [ label [] [ text (gettext "Button Preview" appState.locale) ]
                , div [ class "mt-4" ]
                    [ ExternalLoginButton.render [] buttonName buttonIcon buttonColor buttonBackground
                    ]
                ]
            ]
        ]
