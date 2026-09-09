module Wizard.Api.Models.DocumentTemplateSuggestionTest exposing (decoderTest, languageOptionsTest)

import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Wizard.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion


{-| A suggestion payload without the translation related fields, i.e. what an API
that does not know about document template locales yet returns.
-}
suggestionWithoutTranslationsRaw : String
suggestionWithoutTranslationsRaw =
    """
    {
        "uuid": "1c9ec4b7-1f9e-4f2a-8f0e-2d3a5b6c7d8e",
        "name": "Default Document Template",
        "description": "",
        "organizationId": "dsw",
        "templateId": "default",
        "version": "1.0.0",
        "formats": []
    }
    """


suggestionWithTranslationsRaw : String
suggestionWithTranslationsRaw =
    """
    {
        "uuid": "1c9ec4b7-1f9e-4f2a-8f0e-2d3a5b6c7d8e",
        "name": "Default Document Template",
        "description": "",
        "organizationId": "dsw",
        "templateId": "default",
        "version": "1.0.0",
        "formats": [],
        "language": "en",
        "locales": [
            { "uuid": "2a7b3c4d-5e6f-4a8b-9c0d-1e2f3a4b5c6d", "code": "nl", "name": "Dutch" },
            { "uuid": "3b8c4d5e-6f7a-4b9c-8d0e-2f3a4b5c6d7e", "code": "cs", "name": "Czech" }
        ]
    }
    """


decoderTest : Test
decoderTest =
    describe "DocumentTemplateSuggestion.decoder"
        [ test "decodes a suggestion with the translation fields" <|
            \_ ->
                D.decodeString DocumentTemplateSuggestion.decoder suggestionWithTranslationsRaw
                    |> Result.map (\suggestion -> ( suggestion.language, List.map .code suggestion.locales ))
                    |> Expect.equal (Ok ( "en", [ "nl", "cs" ] ))
        , test "fails when the translation fields are missing" <|
            \_ ->
                D.decodeString DocumentTemplateSuggestion.decoder suggestionWithoutTranslationsRaw
                    |> Expect.err
        ]


languageOptionsTest : Test
languageOptionsTest =
    describe "DocumentTemplateSuggestion.languageOptions"
        [ test "puts the default language first and sorts the locales by code" <|
            \_ ->
                D.decodeString DocumentTemplateSuggestion.decoder suggestionWithTranslationsRaw
                    |> Result.map DocumentTemplateSuggestion.languageOptions
                    |> Expect.equal (Ok [ ( "en", "en" ), ( "cs", "cs" ), ( "nl", "nl" ) ])
        ]
