module Common.Api.Locales exposing
    ( deleteLocale
    , getLocales
    , getLocalesSuggestions
    , getMailTemplatePot
    , getMailTemplatePotInfo
    , getPo
    , getRegistryLocales
    , pullRegistryLocale
    , putLocale
    , uploadLocale
    )

{-| The translations of the user interface (`/locales`) and of the mails
(`/mail-template-locales`): listed, uploaded as a PO, changed, deleted,
downloaded, and pulled from the registry where the server has one. The same
calls for both, by `LocaleKind`.
-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Locale as Locale exposing (Locale)
import Common.Api.Models.Locale.LocaleUploadResult as LocaleUploadResult exposing (LocaleUploadResult)
import Common.Api.Models.Locale.MailTemplatePotInfo as MailTemplatePotInfo exposing (MailTemplatePotInfo)
import Common.Api.Models.Locale.RegistryLocale as RegistryLocale exposing (RegistryLocale)
import Common.Api.Models.LocaleSuggestion as LocaleSuggestion exposing (LocaleSuggestion)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.LocaleKind as LocaleKind exposing (LocaleKind)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


{-| Sorted by name; built-in English is not among them.
-}
getLocales : Context ctx -> LocaleKind -> ToMsg (List Locale) msg -> Cmd msg
getLocales appState kind =
    Request.get (Context.toServerInfo appState) (LocaleKind.apiPath kind) (D.list Locale.decoder)


{-| The enabled translations of the user interface, for the language picker.
-}
getLocalesSuggestions : Context ctx -> ToMsg (List LocaleSuggestion) msg -> Cmd msg
getLocalesSuggestions appState =
    Request.get (Context.toServerInfo appState) "/locales/suggestions" (D.list LocaleSuggestion.decoder)


{-| A PO, first without `confirmed` (what the server read and warns about,
nothing stored), then with it.
-}
uploadLocale : Context ctx -> LocaleKind -> { file : File, confirmed : Bool } -> ToMsg LocaleUploadResult msg -> Cmd msg
uploadLocale appState kind { file, confirmed } =
    Request.postMultiPartWithData (Context.toServerInfo appState)
        (LocaleKind.apiPath kind)
        (Http.filePart "file" file
            :: (if confirmed then
                    [ Http.stringPart "confirmed" "true" ]

                else
                    []
               )
        )
        LocaleUploadResult.decoder


putLocale : Context ctx -> LocaleKind -> { a | uuid : Uuid, enabled : Bool, defaultLocale : Bool } -> ToMsg Locale msg -> Cmd msg
putLocale appState kind locale =
    Request.put (Context.toServerInfo appState)
        (localePath kind locale.uuid)
        Locale.decoder
        (E.object
            [ ( "enabled", E.bool locale.enabled )
            , ( "defaultLocale", E.bool locale.defaultLocale )
            ]
        )


deleteLocale : Context ctx -> LocaleKind -> Uuid -> ToMsg () msg -> Cmd msg
deleteLocale appState kind uuid =
    Request.delete (Context.toServerInfo appState) (localePath kind uuid)


{-| Where the translation's PO can be downloaded from.
-}
getPo : Context ctx -> LocaleKind -> Uuid -> ToMsg UrlResponse msg -> Cmd msg
getPo appState kind uuid =
    Request.get (Context.toServerInfo appState) (localePath kind uuid ++ "/po") UrlResponse.decoder


getRegistryLocales : Context ctx -> LocaleKind -> ToMsg (List RegistryLocale) msg -> Cmd msg
getRegistryLocales appState kind =
    Request.get (Context.toServerInfo appState) (LocaleKind.apiPath kind ++ "/registry") (D.list RegistryLocale.decoder)


pullRegistryLocale : Context ctx -> LocaleKind -> { code : String, confirmed : Bool } -> ToMsg LocaleUploadResult msg -> Cmd msg
pullRegistryLocale appState kind { code, confirmed } =
    Request.post (Context.toServerInfo appState)
        (LocaleKind.apiPath kind ++ "/registry/" ++ code ++ "/pull")
        LocaleUploadResult.decoder
        (E.object [ ( "confirmed", E.bool confirmed ) ])


{-| Where the POT of the templates the mails use can be downloaded from; 404
while there is none.
-}
getMailTemplatePot : Context ctx -> ToMsg UrlResponse msg -> Cmd msg
getMailTemplatePot appState =
    Request.get (Context.toServerInfo appState) "/mail-template-locales/pot" UrlResponse.decoder


getMailTemplatePotInfo : Context ctx -> ToMsg MailTemplatePotInfo msg -> Cmd msg
getMailTemplatePotInfo appState =
    Request.get (Context.toServerInfo appState) "/mail-template-locales/pot/info" MailTemplatePotInfo.decoder


localePath : LocaleKind -> Uuid -> String
localePath kind uuid =
    LocaleKind.apiPath kind ++ "/" ++ Uuid.toString uuid
