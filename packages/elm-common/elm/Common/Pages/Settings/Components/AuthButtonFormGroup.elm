module Common.Pages.Settings.Components.AuthButtonFormGroup exposing (formGroup)

{-| The look of an external login button — its icon and colors — with a
preview of the button as the login screen will show it.

The preview is the real `Common.Components.ExternalLoginButton`, so it shows
the colors exactly as the login screen will: as configured, with no contrast
check (the provider's brand is the administrator's call). It is drawn inert — it is a picture of a
button, not one to press — and, until a name is typed, says "Name" rather than
nothing. Its text is repeated as plain text for a screen reader, which the inert
picture is hidden from.

-}

import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Extra as String
import Ui.Field as UiField
import Ui.Layout as Layout
import Ui.Text as Text
import Uuid


formGroup : AppState -> Form FormError a -> Html Form.Msg
formGroup appState form =
    let
        value field =
            (Form.getFieldAsString field form).value

        service =
            { uuid = Uuid.nil
            , name = Maybe.withDefault "" (value "name")
            , style =
                { icon = Maybe.andThen String.toMaybe (value "styleIcon")
                , color = Maybe.andThen String.toMaybe (value "styleColor")
                , background = Maybe.andThen String.toMaybe (value "styleBackground")
                }
            }
    in
    Layout.stack
        [ Field.input appState form "styleIcon" (gettext "Icon" appState.locale)
            |> Field.withPlaceholder "fab fa-openid"
            |> Field.toHtml
        , Layout.grid
            [ Field.color appState form "styleBackground" (gettext "Background Color" appState.locale)
                |> Field.withPlaceholder "#333"
                |> Field.toHtml
            , Field.color appState form "styleColor" (gettext "Text Color" appState.locale)
                |> Field.withPlaceholder "#fff"
                |> Field.toHtml
            ]
        , UiField.readOnlyHtml
            { id = "form-button-preview"
            , label = gettext "Button Preview" appState.locale
            , value =
                [ ExternalLoginButton.preview
                    { service = service
                    , placeholderName = gettext "Name" appState.locale
                    , noop = Form.NoOp
                    }
                , Text.visuallyHidden (String.toMaybe service.name |> Maybe.withDefault (gettext "Name" appState.locale))
                ]
            }
            |> UiField.toHtml
        ]
