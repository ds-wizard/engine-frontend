module Wizard.Api.Models.ProjectSettingsTest exposing (decoderTest)

import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Wizard.Api.Models.ProjectSettings as ProjectSettings


documentTemplateRaw : String
documentTemplateRaw =
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
            { "uuid": "2a7b3c4d-5e6f-4a8b-9c0d-1e2f3a4b5c6d", "code": "nl", "name": "Dutch" }
        ]
    }
    """


{-| Project settings as returned by the API, parametrized by the `documentTemplate`
value so the tests can vary just that part.
-}
projectSettingsRaw : String -> String
projectSettingsRaw documentTemplate =
    """
    {
        "uuid": "4c9d5e6f-7a8b-4c9d-8e0f-3a4b5c6d7e8f",
        "name": "My Project",
        "description": null,
        "knowledgeModelPackage": {
            "uuid": "5d0e6f7a-8b9c-4d0e-8f1a-4b5c6d7e8f9a",
            "name": "Common Knowledge Model",
            "organizationId": "dsw",
            "kmId": "common",
            "version": "1.0.0",
            "description": "",
            "language": "en",
            "organization": null,
            "remoteLatestVersion": null,
            "phase": "ReleasedKnowledgeModelPackagePhase",
            "createdAt": "2026-09-09T10:00:00.000Z",
            "nonEditable": false,
            "public": false
        },
        "knowledgeModelState": "UpToDateKnowledgeModelProjectState",
        "projectTags": [],
        "selectedQuestionTagUuids": [],
        "documentTemplate": """ ++ documentTemplate ++ """,
        "documentTemplatePhase": null,
        "documentTemplateSupportState": null,
        "documentTemplateState": null,
        "formatUuid": null,
        "documentTemplateLanguage": "en",
        "isTemplate": false,
        "knowledgeModelTags": [],
        "language": "en",
        "availableLocales": []
    }
    """


decoderTest : Test
decoderTest =
    describe "ProjectSettings.decoder"
        [ test "decodes the selected document template" <|
            \_ ->
                D.decodeString ProjectSettings.decoder (projectSettingsRaw documentTemplateRaw)
                    |> Result.map (.documentTemplate >> Maybe.map .name)
                    |> Expect.equal (Ok (Just "Default Document Template"))
        , test "decodes a project without a document template" <|
            \_ ->
                D.decodeString ProjectSettings.decoder (projectSettingsRaw "null")
                    |> Result.map .documentTemplate
                    |> Expect.equal (Ok Nothing)
        , test "fails on a malformed document template instead of dropping it" <|
            \_ ->
                -- With D.maybe here, a document template the frontend cannot parse would
                -- silently decode as Nothing and show up as no template selected.
                D.decodeString ProjectSettings.decoder (projectSettingsRaw """{ "uuid": "1c9ec4b7-1f9e-4f2a-8f0e-2d3a5b6c7d8e" }""")
                    |> Expect.err
        ]
