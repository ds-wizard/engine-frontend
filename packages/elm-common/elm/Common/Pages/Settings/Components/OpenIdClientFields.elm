module Common.Pages.Settings.Components.OpenIdClientFields exposing (focusProviderTab, view)

{-| The fields of an OpenID configuration, the same when it is created and when
it is edited.

The provider type — Microsoft or a custom provider — is `Ui.Tabs`: it swaps the
fields in place and changes no URL. Its value is a field of the form
(`formMode`), so choosing a tab is a form change like any other; the page
returns `focusProviderTab` for it, so the arrow keys move focus with the
selection.

-}

import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)
import Common.Pages.Settings.Components.AuthButtonFormGroup as AuthButtonFormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Form as UiForm
import Ui.Layout as Layout
import Ui.Separator as Separator
import Ui.Tabs as Tabs


{-| `afterName` is what the page shows between the name and the provider — the
callback URLs of a configuration that exists.
-}
view :
    AppState
    ->
        { form : Form FormError OpenIdClientForm
        , toMsg : Form.Msg -> msg
        , afterName : List (Html msg)
        }
    -> List (Html msg)
view appState config =
    let
        form =
            config.form

        field =
            Html.map config.toMsg << Field.toHtml

        isMicrosoftMode =
            OpenIdClientForm.isMicrosoftMode form

        ( clientIdLabel, clientSecretLabel ) =
            if isMicrosoftMode then
                ( gettext "Application (client) ID" appState.locale
                , gettext "Client Secret Value" appState.locale
                )

            else
                ( gettext "Client ID" appState.locale
                , gettext "Client Secret" appState.locale
                )

        microsoftLabel =
            gettext "Microsoft" appState.locale

        customLabel =
            gettext "Custom" appState.locale

        modeFields =
            if isMicrosoftMode then
                [ field (Field.input appState form "directoryId" (gettext "Directory (tenant) ID" appState.locale)) ]

            else
                [ field (Field.input appState form "url" (gettext "URL" appState.locale))
                , Html.map config.toMsg <|
                    Field.list appState
                        form
                        { field = "parameters"
                        , label = gettext "Parameters" appState.locale
                        , addLabel = gettext "Add parameter" appState.locale
                        , rowLabel = \index -> Maybe.withDefault "" (Form.getFieldAsString (Field.listItemPath "parameters" index "name") form).value
                        , row = parameterRow appState form
                        }
                ]
    in
    field (Field.input appState form "name" (gettext "Name" appState.locale))
        :: config.afterName
        ++ [ Html.map config.toMsg <|
                Tabs.toHtml
                    { id = tabsId
                    , label = gettext "Provider" appState.locale
                    , active = isMicrosoftMode
                    , tabs =
                        [ Tabs.tab { id = True, label = microsoftLabel, onSelect = OpenIdClientForm.setFormModeMicrosoftMsg }
                        , Tabs.tab { id = False, label = customLabel, onSelect = OpenIdClientForm.setFormModeCustomMsg }
                        ]
                    , full = False
                    }
           , Tabs.panel
                { id = tabsId
                , label =
                    if isMicrosoftMode then
                        microsoftLabel

                    else
                        customLabel
                }
                [ Layout.stack
                    (Layout.grid
                        [ field (Field.input appState form "clientId" clientIdLabel)
                        , field (Field.secret appState form "clientSecret" clientSecretLabel)
                        ]
                        :: modeFields
                    )
                ]
           , -- Folded away, as the wizard had it: most providers need none of it.
             UiForm.collapsible
                { label = gettext "Advanced configuration" appState.locale
                , content =
                    [ field
                        (Field.toggle appState form "registrationEnabled" (gettext "Registration enabled" appState.locale)
                            |> Field.withHint (gettext "Allow users to register using this OpenID provider." appState.locale)
                        )
                    , field
                        (Field.toggle appState form "scopeEmail" (gettext "Email" appState.locale)
                            |> Field.withHint (gettext "Allows the application to access your email address and whether it has been verified." appState.locale)
                        )
                    , field
                        (Field.toggle appState form "scopeProfile" (gettext "Profile" appState.locale)
                            |> Field.withHint (gettext "Allows the application to access the basic profile information (name, username, profile picture, and other personal details)." appState.locale)
                        )
                    ]
                }
           , Separator.line
           , Html.map config.toMsg (AuthButtonFormGroup.formGroup appState form)
           ]


{-| Focus follows the provider tab just selected. Return it from `update` with
every form message; it does nothing for any other.
-}
focusProviderTab : msg -> Form.Msg -> Cmd msg
focusProviderTab noop formMsg =
    if formMsg == OpenIdClientForm.setFormModeMicrosoftMsg then
        Tabs.focusTabAt noop tabsId 0

    else if formMsg == OpenIdClientForm.setFormModeCustomMsg then
        Tabs.focusTabAt noop tabsId 1

    else
        Cmd.none


tabsId : String
tabsId =
    "openid-provider"


parameterRow : AppState -> Form FormError OpenIdClientForm -> Int -> List (Html Form.Msg)
parameterRow appState form index =
    [ Layout.grid
        [ Field.input appState form (Field.listItemPath "parameters" index "name") (gettext "Name" appState.locale)
            |> Field.toHtml
        , Field.input appState form (Field.listItemPath "parameters" index "value") (gettext "Value" appState.locale)
            |> Field.toHtml
        ]
    ]
