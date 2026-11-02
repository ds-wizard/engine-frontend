module Common.Pages.Settings.Locales.Registry exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| Importing a translation from the registry: what the registry
offers for the running version — name, code, version, and "Installed" where a
translation of that language is here already — each imported through the same
warnings and confirmation as an upload.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Locales as LocalesApi
import Common.Api.Models.Locale exposing (Locale)
import Common.Api.Models.Locale.LocaleUploadResult exposing (LocaleUploadResult(..))
import Common.Api.Models.Locale.MailTemplatePotInfo exposing (MailTemplatePotInfo)
import Common.Api.Models.Locale.RegistryLocale exposing (RegistryLocale)
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Pages.Settings.Locales.Common as Common exposing (Confirmation)
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Layout as Layout
import Ui.Modal as Modal
import Ui.Table as Table
import Ui.Text as Text


type alias Model =
    { kind : LocaleKind
    , locales : ActionResult (List RegistryLocale)
    , potInfo : Maybe MailTemplatePotInfo
    , importing : Maybe String
    , importResult : ActionResult ()
    , confirmation : Maybe ( String, Confirmation )
    , imported : Maybe Locale
    }


initialModel : LocaleKind -> Model
initialModel kind =
    { kind = kind
    , locales = Loading
    , potInfo = Nothing
    , importing = Nothing
    , importResult = Unset
    , confirmation = Nothing
    , imported = Nothing
    }


init : AppState -> LocaleKind -> ( Model, Cmd Msg )
init appState kind =
    ( initialModel kind
    , Cmd.batch
        [ LocalesApi.getRegistryLocales appState kind GetLocalesCompleted
        , case kind of
            MailTemplateLocales ->
                LocalesApi.getMailTemplatePotInfo appState GetPotInfoCompleted

            UiLocales ->
                Cmd.none
        ]
    )


type Msg
    = GetLocalesCompleted (Result ApiError (List RegistryLocale))
    | GetPotInfoCompleted (Result ApiError MailTemplatePotInfo)
    | Import String
    | Confirm
    | CancelConfirmation
    | PullCompleted String Bool (Result ApiError LocaleUploadResult)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        pull code confirmed =
            Cmd.map cfg.wrapMsg <|
                LocalesApi.pullRegistryLocale appState model.kind { code = code, confirmed = confirmed } (PullCompleted code confirmed)
    in
    case msg of
        GetLocalesCompleted result ->
            RequestHelpers.applyResult
                { setResult = \r m -> { m | locales = r }
                , defaultError = gettext "Unable to get the translations of the registry." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        GetPotInfoCompleted result ->
            ( { model | potInfo = Result.toMaybe result }, RequestHelpers.getResultCmd cfg.logoutMsg result )

        Import code ->
            ( { model | importing = Just code, importResult = Loading, imported = Nothing }, pull code False )

        Confirm ->
            case model.confirmation of
                Just ( code, _ ) ->
                    ( { model | importResult = Loading }, pull code True )

                Nothing ->
                    ( model, Cmd.none )

        CancelConfirmation ->
            ( { model | confirmation = Nothing, importing = Nothing, importResult = Unset }, Cmd.none )

        PullCompleted code confirmed result ->
            case result of
                Ok (Preview preview) ->
                    let
                        confirmation =
                            Common.confirmation appState model.kind model.potInfo preview
                    in
                    if List.isEmpty confirmation.warnings && not confirmed then
                        ( model, pull code True )

                    else
                        ( { model | importResult = Unset, confirmation = Just ( code, confirmation ) }, Cmd.none )

                Ok (Stored { locale }) ->
                    ( { model | importing = Nothing, importResult = Unset, confirmation = Nothing, imported = Just locale }
                    , Cmd.map cfg.wrapMsg (LocalesApi.getRegistryLocales appState model.kind GetLocalesCompleted)
                    )

                Err error ->
                    ( { model | importing = Nothing, confirmation = Nothing, importResult = Error (Common.uploadErrorMessage appState error) }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Import from Registry" appState.locale)
                [ Button.linkGhost (gettext "Back to the translations" appState.locale) (Routes.toUrl appState (Routes.settingsLocales model.kind)) ]
            , Text.body (gettext "The translations the registry has for this version. Importing one of a language that is here already replaces it." appState.locale)
            , case model.imported of
                Just locale ->
                    Alert.toHtml
                        (Alert.success
                            (String.format (gettext "%s (%s) has been imported. It is disabled until you enable it in the list." appState.locale) [ locale.name, locale.code ])
                        )

                Nothing ->
                    text ""
            , ApiErrorAlert.view appState (ActionResult.map (always "") model.importResult)
            , Section.actionResult appState (viewLocales appState model) model.locales
            , viewConfirmation appState model
            ]
        ]
    , footer = Nothing
    }


viewLocales : AppState -> Model -> List RegistryLocale -> Html Msg
viewLocales appState model locales =
    if List.isEmpty locales then
        Text.muted (gettext "The registry has no translations for this version." appState.locale)

    else
        Table.table
            [ Table.column (gettext "Name" appState.locale)
                (\locale ->
                    if locale.installed then
                        Layout.inline [ text locale.name, Badge.toHtml (Badge.neutral (gettext "Installed" appState.locale)) ]

                    else
                        text locale.name
                )
            , Table.column (gettext "Code" appState.locale) (text << .code)
            , Table.column (gettext "Version" appState.locale) (text << .remoteVersion)
            , Table.actions (gettext "Actions" appState.locale)
                (\locale ->
                    [ Button.secondary (gettext "Import" appState.locale) (Import locale.code)
                        |> Button.small
                        |> Button.withLoading (model.importing == Just locale.code && Maybe.isNothing model.confirmation)
                        |> Button.withAccessibleName (String.format (gettext "Import %s" appState.locale) [ locale.name ])
                    ]
                )
            ]
            locales
            |> Table.withRowTestId (\row -> "registry-locales_row_" ++ row.code)
            |> Table.withCaption (gettext "Translations in the registry" appState.locale)
            |> Table.withWords (UiWords.table appState)
            |> Table.toHtml


viewConfirmation : AppState -> Model -> Html Msg
viewConfirmation appState model =
    Modal.plain
        { title = gettext "Import translation" appState.locale
        , open = Maybe.isJust model.confirmation
        , onClose = CancelConfirmation
        , body =
            case model.confirmation of
                Just ( _, confirmation ) ->
                    [ Common.viewConfirmation appState confirmation ]

                Nothing ->
                    []
        }
        |> Modal.withForm { onSubmit = Confirm, submitLabel = gettext "Confirm import" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.importResult)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "locale-registry-confirm"
        |> Modal.toHtml
