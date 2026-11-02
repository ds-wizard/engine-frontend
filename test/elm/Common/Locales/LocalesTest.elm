module Common.Locales.LocalesTest exposing (tests)

{-| The translations as the server sends them: the Jed JSON the
application starts with, whatever the PO it was made from left out, and the
answer to an upload.
-}

import Common.Api.Models.Locale.LocaleUploadResult as LocaleUploadResult exposing (LocaleUploadResult(..))
import Expect
import Gettext
import Json.Decode as D
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Translations"
        [ describe "Gettext.localeDecoder"
            [ test "reads a translation" <|
                \_ ->
                    D.decodeString Gettext.localeDecoder (jed """ "plural_forms": "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;" """ """ "Projects": ["Projekty"] """)
                        |> Result.map (Gettext.gettext "Projects")
                        |> Expect.equal (Ok "Projekty")
            , test "a PO without Plural-Forms is read with the English rule" <|
                \_ ->
                    D.decodeString Gettext.localeDecoder (jed "" """ "%s project": ["%s projekt", "%s projekty"] """)
                        |> Result.map (Gettext.ngettext ( "%s project", "%s projects" ) 2)
                        |> Expect.equal (Ok "%s projekty")
            , test "a null form is no translation, not one that cannot be read" <|
                \_ ->
                    D.decodeString Gettext.localeDecoder (jed "" """ "Projects": [null], "Project": ["Projekt"] """)
                        |> Result.map (\locale -> ( Gettext.gettext "Projects" locale, Gettext.gettext "Project" locale ))
                        |> Expect.equal (Ok ( "Projects", "Projekt" ))
            , test "a context key is harmless" <|
                \_ ->
                    D.decodeString Gettext.localeDecoder (jed "" """ "menu\\u0004Open": ["Otevřít"], "Open": ["Otevři"] """)
                        |> Result.map (Gettext.gettext "Open")
                        |> Expect.equal (Ok "Otevři")
            ]
        , describe "LocaleUploadResult"
            [ test "a preview with the translation it would replace" <|
                \_ ->
                    D.decodeString LocaleUploadResult.decoder
                        """{"type": "Preview", "name": "Deutsch", "code": "de", "potVersion": "dsw-client:4.31.0",
                            "warnings": [{"type": "Replace", "uuid": "44444444-4444-4444-8444-444444444441", "name": "Deutsch", "code": "de", "official": true}]}"""
                        |> Result.map
                            (\result ->
                                case result of
                                    Preview preview ->
                                        ( preview.code, List.map .official preview.warnings )

                                    Stored _ ->
                                        ( "stored", [] )
                            )
                        |> Expect.equal (Ok ( "de", [ True ] ))
            , test "a stored translation" <|
                \_ ->
                    D.decodeString LocaleUploadResult.decoder
                        """{"type": "Stored", "replaced": false, "locale": {"uuid": "44444444-4444-4444-8444-444444444441", "code": "fr", "name": "Français",
                            "potVersion": null, "enabled": false, "defaultLocale": false, "official": false,
                            "createdAt": "2026-10-08T10:00:00Z", "updatedAt": "2026-10-08T10:00:00Z"}}"""
                        |> Result.map
                            (\result ->
                                case result of
                                    Stored { locale } ->
                                        locale.name

                                    Preview _ ->
                                        "preview"
                            )
                        |> Expect.equal (Ok "Français")
            ]
        ]


{-| Jed JSON with the given header fields and messages.
-}
jed : String -> String -> String
jed header messages =
    """{"domain": "messages", "locale_data": {"messages": {"": {"domain": "messages" """
        ++ (if String.isEmpty header then
                ""

            else
                ", " ++ header
           )
        ++ "}, "
        ++ messages
        ++ "}}}"
