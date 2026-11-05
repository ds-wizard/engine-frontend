module Common.Pages.Settings.LookAndFeel exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , isLoaded
    , subscriptions
    , update
    , view
    )

{-| The look of the instance: its title and its color, a settings section like
the others. A client whose server takes a logo shows that under it, from a page
of its own that wraps this one.
-}

import ActionResult
import Common.Api.Models.Settings.LookAndFeelSettings as LookAndFeelSettings exposing (LookAndFeelSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.ColorPalette as ColorPalette
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Layout as Layout


type alias Model =
    { genericModel : SettingsPage.Model LookAndFeelSettings LookAndFeelSettings
    }


type Msg
    = GenericMsg (SettingsPage.Msg LookAndFeelSettings)


initialModel : Model
initialModel =
    { genericModel = SettingsPage.initialModel LookAndFeelSettings.initEmptyForm
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


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.map GenericMsg (SettingsPage.subscriptions appState model.genericModel)


{-| The settings have arrived: what is drawn under them waits for it, so it does
not stand alone while they load or after they failed to.
-}
isLoaded : Model -> Bool
isLoaded model =
    ActionResult.isSuccess model.genericModel.config


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    SettingsPage.view appState
        { title = gettext "Look & Feel" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsLookAndFeel)
        , formView = formView appState
        , wrapMsg = GenericMsg
        }
        model.genericModel


formView : AppState -> Form FormError LookAndFeelSettings -> Html (SettingsPage.Msg LookAndFeelSettings)
formView appState form =
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.input appState form "appTitle" (gettext "Application Title" appState.locale)
                |> Field.withPlaceholder appState.config.lookAndFeel.defaultAppTitle
                |> Field.withHint (gettext "Name of the instance (displayed, for example, in the browser tab title, before login or on top of the navigation bar)." appState.locale)
                |> Field.toHtml
            , Field.color appState form "stylePrimaryColor" (gettext "Primary Color" appState.locale)
                |> Field.withSwatches ColorPalette.accents
                |> Field.withClear
                |> Field.withPlaceholder appState.clientDefaults.primaryColor
                |> Field.withHint (gettext "The color the application takes its accents from, as a hex code. Leave it empty to use the default one." appState.locale)
                |> Field.toHtml
            ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model.genericModel)
