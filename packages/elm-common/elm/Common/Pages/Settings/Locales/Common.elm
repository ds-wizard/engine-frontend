module Common.Pages.Settings.Locales.Common exposing
    ( Confirmation
    , clientWarnings
    , confirmation
    , title
    , uploadErrorMessage
    , viewConfirmation
    )

{-| What the pages of the translations of either kind share: their
names, the warnings the client adds to the server's before a translation is
stored, and the confirmation that lists them.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.BuildInfo as BuildInfo
import Common.Api.Models.Locale as Locale
import Common.Api.Models.Locale.LocaleUploadResult exposing (PreviewData)
import Common.Api.Models.Locale.MailTemplatePotInfo exposing (MailTemplatePotInfo)
import Common.Api.ServerError as ServerError
import Common.Data.AppState exposing (AppState)
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.DescriptionList as DescriptionList
import Ui.Layout as Layout
import Ui.Text as Text


title : AppState -> LocaleKind -> String
title appState kind =
    case kind of
        UiLocales ->
            gettext "Application Translations" appState.locale

        MailTemplateLocales ->
            gettext "Email Translations" appState.locale


{-| A translation the server has read and not stored yet, and everything to
say about it before it is: the server's warnings and the client's own.
-}
type alias Confirmation =
    { preview : PreviewData
    , warnings : List String
    }


confirmation : AppState -> LocaleKind -> Maybe MailTemplatePotInfo -> PreviewData -> Confirmation
confirmation appState kind mbPotInfo preview =
    let
        serverWarning warning =
            if warning.official then
                String.format (gettext "%s (%s) is an official translation. After replacing it, we will no longer update it." appState.locale) [ warning.name, warning.code ]

            else
                String.format (gettext "Replaces the existing translation %s (%s)." appState.locale) [ warning.name, warning.code ]
    in
    { preview = preview
    , warnings = List.map serverWarning preview.warnings ++ clientWarnings appState kind mbPotInfo preview.potVersion
    }


{-| What only the client can tell: that the translation was made from another
version than the one it would translate here, or for the other build.
-}
clientWarnings : AppState -> LocaleKind -> Maybe MailTemplatePotInfo -> Maybe String -> List String
clientWarnings appState kind mbPotInfo mbPotVersion =
    let
        ownKind =
            appState.clientDefaults.translationKind

        potKind potVersion =
            case String.split ":" potVersion of
                [ kindPart, _ ] ->
                    Just kindPart

                _ ->
                    Nothing

        runningVersion =
            case kind of
                UiLocales ->
                    Maybe.map (\version -> ownKind ++ ":" ++ version) BuildInfo.clientReleaseVersion

                MailTemplateLocales ->
                    Maybe.andThen .potVersion mbPotInfo

        otherBuild potVersion =
            case ( kind, potKind potVersion ) of
                ( UiLocales, Just translationKind ) ->
                    if translationKind /= ownKind && String.endsWith "-client" translationKind && not (String.isEmpty ownKind) then
                        [ String.format (gettext "Made for another application (%s). Texts that exist only in this one stay in English." appState.locale) [ potVersion ] ]

                    else
                        []

                _ ->
                    []

        otherVersion potVersion =
            case runningVersion of
                Just running ->
                    if Locale.version potVersion /= Locale.version running then
                        [ String.format (gettext "Made for %s, this is %s. Some texts may stay in English." appState.locale) [ potVersion, running ] ]

                    else
                        []

                Nothing ->
                    []
    in
    case mbPotVersion of
        Just potVersion ->
            otherBuild potVersion ++ otherVersion potVersion

        Nothing ->
            []


{-| What was read from the file, and what to know before storing it.
-}
viewConfirmation : AppState -> Confirmation -> Html msg
viewConfirmation appState { preview, warnings } =
    Layout.stack
        [ DescriptionList.toHtml
            [ DescriptionList.withHtml { term = gettext "Name" appState.locale, description = [ text preview.name ] }
            , DescriptionList.withHtml { term = gettext "Code" appState.locale, description = [ text preview.code ] }
            , DescriptionList.withHtml
                { term = gettext "Version" appState.locale
                , description = [ Maybe.unwrap (Text.muted (gettext "Not given" appState.locale)) text preview.potVersion ]
                }
            ]
        , Alert.toHtmlWithContent
            [ Html.ul [] (List.map (\warning -> Html.li [] [ text warning ]) warnings) ]
            (Alert.warning "")
        ]


{-| The server's reason for not taking a file, in words.
-}
uploadErrorMessage : AppState -> ApiError -> String
uploadErrorMessage appState error =
    let
        code =
            case ApiError.toServerError error of
                Just (ServerError.UserSimpleError message) ->
                    Just message.code

                _ ->
                    Nothing
    in
    case code of
        Just "error.validation.locale_po_too_large" ->
            gettext "The file is too large. A translation can be at most 5 MB." appState.locale

        Just "error.validation.locale_po_unparsable" ->
            gettext "The file is not a PO file that can be read." appState.locale

        Just "error.validation.locale_po_missing_language" ->
            gettext "The file does not say its language (the Language header)." appState.locale

        Just "error.validation.locale_po_invalid_language" ->
            gettext "The file's language (the Language header) is not a language code." appState.locale

        Just "error.validation.locale_po_unsupported_kind" ->
            gettext "The file is not a translation of the user interface." appState.locale

        _ ->
            case ApiError.toActionResult appState (gettext "The translation could not be uploaded." appState.locale) error of
                Error message ->
                    message

                _ ->
                    gettext "The translation could not be uploaded." appState.locale
