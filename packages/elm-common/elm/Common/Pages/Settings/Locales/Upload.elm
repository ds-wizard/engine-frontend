module Common.Pages.Settings.Locales.Upload exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| Uploading a translation: one PO file, nothing else to enter.

The file is sent without `confirmed` first: the server reads its header and
says what it would replace, and stores nothing. With nothing to warn about —
neither from the server nor from the client (another version, the other
application) — it is sent again with `confirmed` at once, so the admin sees no
confirmation step. Otherwise the page shows what was read and the warnings, and
stores it only on Confirm upload. A file the server will not take is said under
the field. A new translation is disabled until it is enabled in the list.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Locales as LocalesApi
import Common.Api.Models.Locale exposing (Locale)
import Common.Api.Models.Locale.LocaleUploadResult exposing (LocaleUploadResult(..))
import Common.Api.Models.Locale.MailTemplatePotInfo exposing (MailTemplatePotInfo)
import Common.Components.Form as CommonForm
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Pages.Settings.Locales.Common as Common exposing (Confirmation)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.FileUpload as FileUpload
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Text as Text


type alias Model =
    { kind : LocaleKind
    , upload : FileUpload.Model
    , fileError : Maybe String
    , potInfo : Maybe MailTemplatePotInfo
    , sending : ActionResult ()
    , confirmation : Maybe Confirmation
    , stored : Maybe Locale
    }


initialModel : LocaleKind -> Model
initialModel kind =
    { kind = kind
    , upload = FileUpload.init
    , fileError = Nothing
    , potInfo = Nothing
    , sending = Unset
    , confirmation = Nothing
    , stored = Nothing
    }


{-| The mails' version warning compares with the POT the mails use.
-}
init : AppState -> LocaleKind -> ( Model, Cmd Msg )
init appState kind =
    ( initialModel kind
    , case kind of
        MailTemplateLocales ->
            LocalesApi.getMailTemplatePotInfo appState GetPotInfoCompleted

        UiLocales ->
            Cmd.none
    )


type Msg
    = UploadMsg FileUpload.Msg
    | ClearFile
    | Submit
    | Cancel
    | Confirm
    | CancelConfirmation
    | SendCompleted Bool (Result ApiError LocaleUploadResult)
    | GetPotInfoCompleted (Result ApiError MailTemplatePotInfo)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        send file confirmed =
            Cmd.map cfg.wrapMsg <|
                LocalesApi.uploadLocale appState model.kind { file = file, confirmed = confirmed } (SendCompleted confirmed)
    in
    case msg of
        UploadMsg uploadMsg ->
            let
                ( upload, cmd ) =
                    FileUpload.update { mimes = [ "application/x-po", "text/x-gettext-translation", ".po" ], read = False } uploadMsg model.upload
            in
            ( { model | upload = upload, fileError = Nothing, confirmation = Nothing, sending = Unset }
            , Cmd.map (cfg.wrapMsg << UploadMsg) cmd
            )

        ClearFile ->
            ( { model | upload = FileUpload.clear, fileError = Nothing, confirmation = Nothing, sending = Unset }, Cmd.none )

        Submit ->
            case FileUpload.file model.upload of
                Just file ->
                    ( { model | sending = Loading, fileError = Nothing }, send file False )

                Nothing ->
                    ( { model | fileError = Just (gettext "Choose a PO file to upload." appState.locale) }, Cmd.none )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState (Routes.settingsLocales model.kind)) )

        Confirm ->
            case FileUpload.file model.upload of
                Just file ->
                    ( { model | sending = Loading }, send file True )

                Nothing ->
                    ( model, Cmd.none )

        CancelConfirmation ->
            ( { model | confirmation = Nothing, sending = Unset }, Cmd.none )

        SendCompleted confirmed result ->
            case result of
                Ok (Preview preview) ->
                    let
                        confirmation =
                            Common.confirmation appState model.kind model.potInfo preview
                    in
                    if List.isEmpty confirmation.warnings && not confirmed then
                        case FileUpload.file model.upload of
                            Just file ->
                                ( model, send file True )

                            Nothing ->
                                ( { model | sending = Unset }, Cmd.none )

                    else
                        ( { model | sending = Unset, confirmation = Just confirmation }, Cmd.none )

                Ok (Stored { locale }) ->
                    ( { model | sending = Success (), stored = Just locale, confirmation = Nothing }, Cmd.none )

                Err error ->
                    ( { model | sending = Unset, confirmation = Nothing, fileError = Just (Common.uploadErrorMessage appState error) }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        GetPotInfoCompleted result ->
            ( { model | potInfo = Result.toMaybe result }, RequestHelpers.getResultCmd cfg.logoutMsg result )


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            (Section.header (gettext "Upload translation" appState.locale) [ Section.guideLink appState GuideLinks.settingsTranslationsUpload ]
                :: (case ( model.stored, model.confirmation ) of
                        ( Just locale, _ ) ->
                            viewStored appState model locale

                        ( Nothing, Just confirmation ) ->
                            viewConfirmation appState model confirmation

                        ( Nothing, Nothing ) ->
                            viewFile appState model
                   )
            )
        ]
    , footer = Nothing
    }


viewFile : AppState -> Model -> List (Html Msg)
viewFile appState model =
    [ Text.body
        (case model.kind of
            UiLocales ->
                gettext "Choose the PO file of a translation of the user interface. Its language, name and version are read from the file." appState.locale

            MailTemplateLocales ->
                gettext "Choose the PO file of a translation of the mails. Its language, name and version are read from the file." appState.locale
        )
    , CommonForm.createView
        { onSubmit = Submit
        , content =
            [ FileUpload.toHtmlWithErrorAndTestId (UiWords.fileUpload appState)
                { id = "locale-upload-file"
                , toMsg = UploadMsg
                , label = gettext "PO file" appState.locale
                , buttonLabel = gettext "Select .po file" appState.locale
                , hint = gettext "or drop it here" appState.locale
                , error = model.fileError
                , onClear = Just ClearFile
                , testId = Just "locale-upload-dropzone"
                }
                model.upload
            , CommonForm.actions appState
                { submitLabel = gettext "Upload" appState.locale
                , result = ActionResult.map (always "") model.sending
                , onCancel = Just Cancel
                }
            ]
        }
    ]


viewConfirmation : AppState -> Model -> Confirmation -> List (Html Msg)
viewConfirmation appState model confirmation =
    [ Text.body (gettext "Check what was read from the file before it is stored." appState.locale)
    , Common.viewConfirmation appState confirmation
    , Layout.inline
        [ Button.primary (gettext "Confirm upload" appState.locale) Confirm
            |> Button.withLoading (ActionResult.isLoading model.sending)
            |> Button.withTestId "locale-upload-confirm"
            |> Button.toHtml
        , Button.ghost (gettext "Cancel" appState.locale) CancelConfirmation
            |> Button.toHtml
        ]
    ]


viewStored : AppState -> Model -> Locale -> List (Html Msg)
viewStored appState model locale =
    [ Alert.toHtml
        (Alert.success
            (if locale.enabled then
                String.format (gettext "%s (%s) has been uploaded." appState.locale) [ locale.name, locale.code ]

             else
                String.format (gettext "%s (%s) has been uploaded. It is disabled until you enable it in the list." appState.locale) [ locale.name, locale.code ]
            )
        )
    , Button.linkPrimary (gettext "Back to the translations" appState.locale) (Routes.toUrl appState (Routes.settingsLocales model.kind))
        |> Button.withIcon (Icon.solid "fa-arrow-left")
        |> Button.withTestId "locale-upload-done"
        |> Button.toHtml
    ]
