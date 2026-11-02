module Common.Pages.Settings.LookAndFeel exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The look of the instance: its title, its color and its logo.

The title and the color are a settings section like the others. The logo is a
file: it is shown as the navigation shows it, a new one is chosen with
`Ui.FileUpload` (by button, or by dropping it) and saved on its own, and the
default can be put back while a custom one is set. Either reloads the
application, as a saved setting does, so every place showing the logo shows the
new one.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Common.Api.Models.Settings.LookAndFeelSettings as LookAndFeelSettings exposing (LookAndFeelSettings)
import Common.Api.Settings as SettingsApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Ports.Window as Window
import Common.Utils.ColorPalette as ColorPalette
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Ui.Button as Button
import Ui.Field as UiField
import Ui.FileUpload as FileUpload
import Ui.Form as UiForm
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Text as Text


type alias Model =
    { genericModel : SettingsPage.Model LookAndFeelSettings LookAndFeelSettings
    , upload : FileUpload.Model
    , savingLogo : ActionResult ()
    }


type Msg
    = GenericMsg (SettingsPage.Msg LookAndFeelSettings)
    | FileUploadMsg FileUpload.Msg
    | ClearFile
    | SaveLogo
    | UseDefaultLogo
    | SaveLogoComplete (Result ApiError ())


initialModel : Model
initialModel =
    { genericModel = SettingsPage.initialModel LookAndFeelSettings.initEmptyForm
    , upload = FileUpload.init
    , savingLogo = Unset
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    let
        ( genericModel, genericCmd ) =
            SettingsPage.init appState LookAndFeelSettings.section LookAndFeelSettings.initEmptyForm
    in
    ( { initialModel | genericModel = genericModel }
    , Cmd.map GenericMsg genericCmd
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GenericMsg genericMsg ->
            let
                ( genericModel, cmd ) =
                    SettingsPage.update appState
                        { wrapMsg = cfg.wrapMsg << GenericMsg
                        , logoutMsg = cfg.logoutMsg
                        , section = LookAndFeelSettings.section
                        , initForm = LookAndFeelSettings.initForm
                        , formToValue = \form _ -> form
                        , formValidation = LookAndFeelSettings.validation
                        }
                        genericMsg
                        model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        FileUploadMsg uploadMsg ->
            let
                ( upload, cmd ) =
                    FileUpload.update { mimes = [ "image/*" ], read = False } uploadMsg model.upload
            in
            ( { model | upload = upload }, Cmd.map (cfg.wrapMsg << FileUploadMsg) cmd )

        ClearFile ->
            ( { model | upload = FileUpload.clear }, Cmd.none )

        SaveLogo ->
            case FileUpload.file model.upload of
                Just file ->
                    ( { model | savingLogo = Loading }
                    , Cmd.map cfg.wrapMsg (SettingsApi.putLogo appState file SaveLogoComplete)
                    )

                Nothing ->
                    ( model, Cmd.none )

        UseDefaultLogo ->
            ( { model | savingLogo = Loading }
            , Cmd.map cfg.wrapMsg (SettingsApi.deleteLogo appState SaveLogoComplete)
            )

        SaveLogoComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingLogo = Success () }, Window.refresh () )

                Err error ->
                    ( { model | savingLogo = ApiError.toActionResult appState (gettext "Unable to upload logo." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.map GenericMsg (SettingsPage.subscriptions appState model.genericModel)


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    let
        generic =
            SettingsPage.view appState
                { title = gettext "Look & Feel" appState.locale
                , description = Nothing
                , guideLink = Just (Section.guideLink appState GuideLinks.settingsLookAndFeel)
                , formView = formView appState
                , wrapMsg = GenericMsg
                }
                model.genericModel

        logo =
            if ActionResult.isSuccess model.genericModel.config then
                [ logoView appState model ]

            else
                []
    in
    { body = generic.body ++ logo
    , footer = generic.footer
    }


formView : AppState -> Form FormError LookAndFeelSettings -> Html (SettingsPage.Msg LookAndFeelSettings)
formView appState form =
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.input appState form "appTitle" (gettext "Application Title" appState.locale)
                |> Field.withPlaceholder appState.config.lookAndFeel.defaultAppTitle
                |> Field.withHint (gettext "Name of the DSW instance (displayed, for example, in the browser tab title, before login or on top of the navigation bar)." appState.locale)
                |> Field.toHtml
            , Field.color appState form "stylePrimaryColor" (gettext "Primary Color" appState.locale)
                |> Field.withSwatches ColorPalette.accents
                |> Field.withClear
                |> Field.withPlaceholder appState.clientDefaults.primaryColor
                |> Field.withHint (gettext "The color the application takes its accents from, as a hex code. Leave it empty to use the default one." appState.locale)
                |> Field.toHtml
            ]


{-| The logo is not part of the form above: it is saved on its own, so it has
its own buttons, and sits in a section of its own under the form.
-}
logoView : AppState -> Model -> Html Msg
logoView appState model =
    let
        lookAndFeel =
            appState.config.lookAndFeel

        current =
            UiField.readOnlyHtml
                { id = "settings-logo-current"
                , label = gettext "Current logo" appState.locale
                , value =
                    [ Layout.inline
                        [ ItemIcon.view (LookAndFeelConfig.getAppTitle lookAndFeel)
                            |> ItemIcon.withImage (Just (LookAndFeelConfig.getLogoUrl lookAndFeel))
                            |> ItemIcon.withSize ItemIcon.Large
                            |> ItemIcon.toHtml
                        , Text.body (LookAndFeelConfig.getAppTitle lookAndFeel)
                        ]
                    ]
                }
                |> UiField.toHtml

        busy =
            ActionResult.isLoading model.savingLogo || ActionResult.isSuccess model.savingLogo

        useDefault =
            if Maybe.isJust lookAndFeel.logoUrl then
                [ Button.secondary (gettext "Use default logo" appState.locale) UseDefaultLogo
                    |> Button.withLoading busy
                    |> Button.toHtml
                ]

            else
                []

        save =
            if Maybe.isJust (FileUpload.file model.upload) then
                [ Button.primary (gettext "Save logo" appState.locale) SaveLogo
                    |> Button.withLoading busy
                    |> Button.toHtml
                ]

            else
                []
    in
    UiForm.section
        { id = "settings-logo"
        , legend = gettext "Logo" appState.locale
        , content =
            [ Text.body (gettext "Logo is used next to the application name in the menu. It is recommended to use a square image." appState.locale)
            , ApiErrorAlert.view appState model.savingLogo
            , current
            , FileUpload.toHtmlWithWords (UiWords.fileUpload appState)
                { toMsg = FileUploadMsg
                , label = gettext "Change logo" appState.locale
                , buttonLabel = gettext "Choose logo" appState.locale
                , hint = gettext "Or just drop it here" appState.locale
                , invalid = False
                , onClear = Just ClearFile
                }
                model.upload
            , Layout.inline (save ++ useDefault)
            ]
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState
        (SettingsPage.isDirty model.genericModel
            || (Maybe.isJust (FileUpload.file model.upload) && not (ActionResult.isSuccess model.savingLogo))
        )
