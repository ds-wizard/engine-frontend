module Common.Pages.Settings.LoginScreen exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.LoginScreenSettings as LoginScreenSettings exposing (LoginScreenSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.AnnouncementFields as AnnouncementFields
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.GuideLinks as GuideLinksUtils
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model LoginScreenSettings LoginScreenSettings


type alias Msg =
    SettingsPage.Msg LoginScreenSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel LoginScreenSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState LoginScreenSettings.section LoginScreenSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = LoginScreenSettings.section
        , initForm = LoginScreenSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = LoginScreenSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Login Screen" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsLoginScreen)
        , formView = formView appState
        , wrapMsg = identity
        }


{-| The two infos are the one place where HTML is written as well as Markdown:
the login screen renders them with `MarkdownOrHtml`, and instances have custom
HTML there. They are edited as source, with a preview of what the login screen
shows (`Field.markdownOrHtml`), not in the Markdown editor, which would escape
the HTML.
-}
formView : AppState -> Form FormError LoginScreenSettings -> Html Msg
formView appState form =
    let
        -- The login screen takes the text as HTML if there is a tag anywhere
        -- in it, and as Markdown otherwise (`MarkdownOrHtml`), so a mix shows
        -- its Markdown as typed. The hint says so; the preview shows it.
        markupHint =
            String.format (gettext "Use either HTML or [Markdown](%s), not both: if the text contains any HTML tag, all of it is treated as HTML and Markdown in it is shown as typed. Check the result in the preview." appState.locale)
                [ GuideLinksUtils.wrap (AppState.toServerInfo appState) (GuideLinks.markdownCheatsheet appState.guideLinks) ]
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.markdownOrHtml appState form "loginInfo" (gettext "Login Info" appState.locale)
                |> Field.withHintMarkdown (gettext "Additional information displayed on the login screen next to the login form." appState.locale ++ " " ++ markupHint)
                |> Field.toHtml
            , Field.markdownOrHtml appState form "loginInfoSidebar" (gettext "Sidebar Login Info" appState.locale)
                |> Field.withHintMarkdown (gettext "Additional information displayed on the login screen underneath the login form." appState.locale ++ " " ++ markupHint)
                |> Field.toHtml
            , AnnouncementFields.view appState
                form
                { field = "announcements"
                , hint = gettext "Announcements are displayed on the login screen, visible to everybody." appState.locale
                }
            ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
