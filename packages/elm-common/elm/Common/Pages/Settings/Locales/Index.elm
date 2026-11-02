module Common.Pages.Settings.Locales.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| The translations of a kind: built-in English first, apart from
the rest, with its POT to translate from; then the uploaded translations, each
with its version, whether it is ours (Official), the default or disabled, and
its actions — set or unset as the default, enable or disable, download its PO,
delete.

The mails' translations also say which templates the mails use (the default
ones or the organization's own) and their POT's version, which is the POT to
download.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Locales as LocalesApi
import Common.Api.Models.Locale exposing (Locale)
import Common.Api.Models.Locale.MailTemplatePotInfo exposing (MailTemplatePotInfo, PotSource(..))
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Timestamp as Timestamp
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Pages.Settings.Locales.Common as Common
import Common.Ports.File as FilePorts
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import File.Download as Download
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Http
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button exposing (Button)
import Ui.Card as Card
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Table as Table
import Ui.Text as Text
import Uuid



-- MODEL


type alias Model =
    { kind : LocaleKind
    , locales : ActionResult (List Locale)
    , potInfo : ActionResult MailTemplatePotInfo
    , changing : ActionResult ()
    , toDelete : Maybe Locale
    , deleting : ActionResult String
    , downloading : ActionResult ()
    }


initialModel : LocaleKind -> Model
initialModel kind =
    { kind = kind
    , locales = Loading
    , potInfo = Unset
    , changing = Unset
    , toDelete = Nothing
    , deleting = Unset
    , downloading = Unset
    }


init : AppState -> LocaleKind -> ( Model, Cmd Msg )
init appState kind =
    let
        model =
            initialModel kind
    in
    case kind of
        UiLocales ->
            ( model, LocalesApi.getLocales appState kind GetLocalesCompleted )

        MailTemplateLocales ->
            ( { model | potInfo = Loading }
            , Cmd.batch
                [ LocalesApi.getLocales appState kind GetLocalesCompleted
                , LocalesApi.getMailTemplatePotInfo appState GetPotInfoCompleted
                ]
            )



-- UPDATE


type Msg
    = GetLocalesCompleted (Result ApiError (List Locale))
    | GetPotInfoCompleted (Result ApiError MailTemplatePotInfo)
    | Change Locale { enabled : Bool, defaultLocale : Bool }
    | ChangeCompleted (Result ApiError Locale)
    | DownloadPo Locale
    | DownloadPot
    | DownloadUrlCompleted (Result ApiError UrlResponse)
    | DownloadUiPotCompleted (Result Http.Error String)
    | ShowDelete (Maybe Locale)
    | Delete
    | DeleteCompleted (Result ApiError ())
    | FocusDone


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        reload =
            Cmd.map cfg.wrapMsg (LocalesApi.getLocales appState model.kind GetLocalesCompleted)

        focusHeading =
            Cmd.map cfg.wrapMsg (Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId))
    in
    case msg of
        GetLocalesCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | locales = r }
                , defaultError = gettext "Unable to get the translations." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetPotInfoCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | potInfo = r }
                , defaultError = gettext "Unable to get the templates' POT." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        Change locale change ->
            ( { model | changing = Loading }
            , Cmd.map cfg.wrapMsg (LocalesApi.putLocale appState model.kind { uuid = locale.uuid, enabled = change.enabled, defaultLocale = change.defaultLocale } ChangeCompleted)
            )

        ChangeCompleted result ->
            case result of
                Ok _ ->
                    -- Setting a default unsets the other one: the list is read
                    -- again rather than patched.
                    ( { model | changing = Unset }, Cmd.batch [ reload, focusHeading ] )

                Err error ->
                    ( { model | changing = ApiError.toActionResult appState (gettext "The translation could not be changed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        DownloadPo locale ->
            ( { model | downloading = Loading }
            , Cmd.map cfg.wrapMsg (LocalesApi.getPo appState model.kind locale.uuid DownloadUrlCompleted)
            )

        DownloadPot ->
            ( { model | downloading = Loading }
            , case model.kind of
                UiLocales ->
                    -- The POT of the user interface is a file of this build.
                    Cmd.map cfg.wrapMsg (Http.get { url = "/ui.pot", expect = Http.expectString DownloadUiPotCompleted })

                MailTemplateLocales ->
                    Cmd.map cfg.wrapMsg (LocalesApi.getMailTemplatePot appState DownloadUrlCompleted)
            )

        DownloadUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( { model | downloading = Unset }, FilePorts.downloadFile urlResponse.url )

                Err error ->
                    ( { model | downloading = ApiError.toActionResult appState (gettext "The file could not be downloaded." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        DownloadUiPotCompleted result ->
            case result of
                Ok content ->
                    -- A server without the file answers with the application's page;
                    -- a POT has the header entry, after its comments.
                    if String.contains "msgid \"\"" content then
                        ( { model | downloading = Unset }, Download.string "ui.pot" "text/x-gettext-translation-template" content )

                    else
                        ( { model | downloading = Error (gettext "The file could not be downloaded." appState.locale) }, Cmd.none )

                Err _ ->
                    ( { model | downloading = Error (gettext "The file could not be downloaded." appState.locale) }, Cmd.none )

        ShowDelete mbLocale ->
            ( { model | toDelete = mbLocale, deleting = Unset }, Cmd.none )

        Delete ->
            case model.toDelete of
                Just locale ->
                    ( { model | deleting = Loading }
                    , Cmd.map cfg.wrapMsg (LocalesApi.deleteLocale appState model.kind locale.uuid DeleteCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case result of
                Ok _ ->
                    ( { model | toDelete = Nothing, deleting = Unset, locales = Loading }, Cmd.batch [ reload, focusHeading ] )

                Err error ->
                    ( { model | deleting = ApiError.toActionResult appState (gettext "The translation could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )



-- VIEW


{-| `registry` says whether the translations can be imported from the registry
here.
-}
view : AppState -> { registry : Bool } -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState { registry } model =
    let
        actions =
            Button.linkPrimary (gettext "Upload translation" appState.locale) (Routes.toUrl appState (Routes.settingsLocalesUpload model.kind))
                |> Button.withIcon (Icon.solid "fa-upload")
                |> Button.withTestId "locales-upload"
                |> (\upload ->
                        if registry then
                            [ upload
                            , Button.link (gettext "Import from Registry" appState.locale) (Routes.toUrl appState (Routes.settingsLocalesRegistry model.kind))
                                |> Button.withTestId "locales-registry"
                            ]

                        else
                            [ upload ]
                   )
    in
    { body =
        [ Layout.stack
            ([ Section.header (Common.title appState model.kind) actions
             , ApiErrorAlert.view appState model.changing
             , ApiErrorAlert.view appState (ActionResult.map (always "") model.downloading)
             ]
                ++ viewPotInfo appState model
                ++ [ Section.actionResult appState (viewLocales appState model) model.locales
                   , viewDeleteModal appState model
                   ]
            )
        ]
    , footer = Nothing
    }


{-| That the mails have no POT yet, which is why there is none to download.
-}
viewPotInfo : AppState -> Model -> List (Html Msg)
viewPotInfo appState model =
    case ( model.kind, model.potInfo ) of
        ( MailTemplateLocales, Success { source } ) ->
            if source == NoPot then
                [ Alert.toHtml (Alert.info (gettext "The template POT is not available yet." appState.locale)) ]

            else
                []

        ( MailTemplateLocales, Error _ ) ->
            [ ApiErrorAlert.view appState model.potInfo ]

        _ ->
            []


viewLocales : AppState -> Model -> List Locale -> Html Msg
viewLocales appState model locales =
    Layout.stackLoose
        (viewEnglish appState model locales
            :: (if List.isEmpty locales then
                    [ Text.muted
                        (gettext "There are no translations yet. Translate the POT and upload the PO file, and the language can be chosen in the profile once it is enabled." appState.locale)
                    ]

                else
                    [ Table.table
                        [ Table.column (gettext "Name" appState.locale) (viewName appState)
                        , Table.column (gettext "Code" appState.locale) (text << .code)
                        , Table.column (gettext "Version" appState.locale) (viewVersion appState model)
                        , Table.column (gettext "Updated" appState.locale) (Timestamp.view appState << .updatedAt)
                        , Table.actions (gettext "Actions" appState.locale) (viewActions appState)
                        ]
                        locales
                        |> Table.withRowTestId (\row -> "locales_row_" ++ Uuid.toString row.uuid)
                        |> Table.withWords (UiWords.table appState)
                        |> Table.toHtml
                    ]
               )
        )


{-| Built-in English, which is no translation: the default while none is, the
fallback for whatever a translation leaves out, and the POT to translate from.
A card of its own, apart from the heading above and the translations below.
-}
viewEnglish : AppState -> Model -> List Locale -> Html Msg
viewEnglish appState model locales =
    let
        isDefault =
            not (List.any .defaultLocale locales)

        potAvailable =
            case ( model.kind, model.potInfo ) of
                ( MailTemplateLocales, Success potInfo ) ->
                    potInfo.source /= NoPot

                ( MailTemplateLocales, _ ) ->
                    False

                ( UiLocales, _ ) ->
                    True

        downloadPot =
            Button.secondary (gettext "Download POT" appState.locale) DownloadPot
                |> Button.withIcon (Icon.solid "fa-download")
                |> Button.small
                |> Button.withTestId "locales-download-pot"
                |> Button.withDescription { id = "locales-download-pot-description", text = gettext "Use the POT file to create a translation in a gettext editor, then upload the PO file it makes." appState.locale }
                |> (\button ->
                        if potAvailable then
                            button

                        else
                            Button.withDisabledReason { id = "locales-pot-unavailable", reason = gettext "The template POT is not available yet." appState.locale } button
                   )
    in
    Card.view
        [ Layout.inlineBetween
            [ Layout.inline
                (Html.strong [] [ text (gettext "English (built-in)" appState.locale) ]
                    :: (if isDefault then
                            [ Badge.toHtml (Badge.neutral (gettext "Default" appState.locale)) ]

                        else
                            []
                       )
                )
            , Button.toHtml downloadPot
            ]
        ]


viewName : AppState -> Locale -> Html Msg
viewName appState locale =
    Layout.inline
        (text locale.name
            :: Maybe.values
                [ if locale.official then
                    Just (Badge.toHtml (Badge.info (gettext "Official" appState.locale)))

                  else
                    Nothing
                , if locale.defaultLocale then
                    Just (Badge.toHtml (Badge.neutral (gettext "Default" appState.locale)))

                  else
                    Nothing
                , if locale.enabled then
                    Nothing

                  else
                    Just (Badge.toHtml (Badge.warning (gettext "Disabled" appState.locale)))
                ]
        )


{-| The version a translation was made for, marked where it is not the running
one: some of its texts may be missing.
-}
viewVersion : AppState -> Model -> Locale -> Html Msg
viewVersion appState model locale =
    case locale.potVersion of
        Just potVersion ->
            let
                mbPotInfo =
                    case model.potInfo of
                        Success potInfo ->
                            Just potInfo

                        _ ->
                            Nothing
            in
            if List.isEmpty (Common.clientWarnings appState model.kind mbPotInfo (Just potVersion)) then
                text potVersion

            else
                Layout.inline [ text potVersion, Badge.toHtml (Badge.warning (gettext "Another version" appState.locale)) ]

        Nothing ->
            Text.muted "-"


{-| The same four buttons on every row, so the column reads the same down the
list: the default, enabled, its PO and Delete. One that does not apply to the
row is unavailable, with the reason (`Button.withDisabledReason`).
-}
viewActions : AppState -> Locale -> List (Button Msg)
viewActions appState locale =
    let
        named label =
            Button.withAccessibleName (String.format label [ locale.name ])

        reasonId what =
            "locale-" ++ what ++ "-reason-" ++ Uuid.toString locale.uuid

        default =
            if locale.defaultLocale then
                Button.iconOnly (Icon.solid "fa-xmark") (gettext "Unset default" appState.locale) (Change locale { enabled = locale.enabled, defaultLocale = False })
                    |> named (gettext "Unset %s as the default" appState.locale)

            else
                Button.iconOnly (Icon.solid "fa-check") (gettext "Set as default" appState.locale) (Change locale { enabled = True, defaultLocale = True })
                    |> named (gettext "Set %s as the default" appState.locale)
                    |> (if locale.enabled then
                            identity

                        else
                            Button.withDisabledReason { id = reasonId "default", reason = gettext "Enable the translation to make it the default." appState.locale }
                       )

        enabled =
            if locale.enabled then
                Button.iconOnly (Icon.solid "fa-toggle-off") (gettext "Disable" appState.locale) (Change locale { enabled = False, defaultLocale = False })
                    |> named (gettext "Disable %s" appState.locale)
                    |> (if locale.defaultLocale then
                            Button.withDisabledReason { id = reasonId "disable", reason = gettext "The default translation cannot be disabled." appState.locale }

                        else
                            identity
                       )

            else
                Button.iconOnly (Icon.solid "fa-toggle-on") (gettext "Enable" appState.locale) (Change locale { enabled = True, defaultLocale = False })
                    |> named (gettext "Enable %s" appState.locale)

        downloadPo =
            Button.iconOnly (Icon.solid "fa-download") (gettext "Download PO" appState.locale) (DownloadPo locale)
                |> named (gettext "Download PO of %s" appState.locale)

        delete =
            Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (ShowDelete (Just locale))
                |> named (gettext "Delete %s" appState.locale)
    in
    [ default, enabled, downloadPo, delete ]


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        question locale =
            String.format (gettext "Delete %s (%s)? Users who chose it will fall back to the default." appState.locale) [ locale.name, locale.code ]
                ++ (if locale.official then
                        " " ++ gettext "This is an official translation; it will not be added back or updated." appState.locale

                    else
                        ""
                   )
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete translation" appState.locale
        , open = Maybe.isJust model.toDelete
        , question = Maybe.unwrap "" question model.toDelete
        , entity = text ""
        , onConfirm = Delete
        , onClose = ShowDelete Nothing
        , result = model.deleting
        , testId = Just "locale-delete"
        }
