module Common.Pages.Settings.Support exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.SupportSettings as SupportSettings exposing (SupportSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.FontAwesome as FontAwesome
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Icon as Icon
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model SupportSettings SupportSettings


type alias Msg =
    SettingsPage.Msg SupportSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel SupportSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState SupportSettings.section SupportSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = SupportSettings.section
        , initForm = SupportSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = SupportSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Support" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsPrivacyAndSupport)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError SupportSettings -> Html Msg
formView appState form =
    let
        -- The icon the help modal will show, drawn as it is typed: the default
        -- one while the field is empty.
        supportSiteIcon =
            (Form.getFieldAsString "supportSiteIcon" form).value
                |> Maybe.filter (not << String.isEmpty << String.trim)
                |> Maybe.withDefault appState.clientDefaults.supportSiteIcon
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.email appState form "supportEmail" (gettext "Support Email" appState.locale)
                |> Field.withPlaceholder appState.clientDefaults.supportEmail
                |> Field.withHint (gettext "Support email displayed in the help modal." appState.locale)
                |> Field.toHtml
            , Field.input appState form "supportSiteName" (gettext "Support Site Name" appState.locale)
                |> Field.withPlaceholder appState.clientDefaults.supportSiteName
                |> Field.withHint (gettext "Name of the support site where users can report issues related to the service." appState.locale)
                |> Field.toHtml
            , Field.input appState form "supportSiteUrl" (gettext "Support Site URL" appState.locale)
                |> Field.withPlaceholder appState.clientDefaults.supportSiteUrl
                |> Field.withHint (gettext "URL of the support site where users can report issues related to the service." appState.locale)
                |> Field.toHtml
            , Field.input appState form "supportSiteIcon" (gettext "Support Site Icon" appState.locale)
                |> Field.withPlaceholder appState.clientDefaults.supportSiteIcon
                |> Field.withSuffixIcon (Icon.fromClass supportSiteIcon)
                |> Field.withHintMarkdown (String.format (gettext "Icon of the support site using [Font Awesome](%s)." appState.locale) [ FontAwesome.link ])
                |> Field.toHtml
            ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
