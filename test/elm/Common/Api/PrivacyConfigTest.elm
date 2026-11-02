module Common.Api.PrivacyConfigTest exposing (privacyConfigTests)

import Common.Api.Models.BootstrapConfig.PrivacyConfig as PrivacyConfig exposing (PrivacyConfig)
import Expect
import Test exposing (Test, describe, test)


deployment : PrivacyConfig
deployment =
    { privacyUrl = Just "https://example.com/privacy-policy"
    , termsOfServiceUrl = Just "https://example.com/terms-of-service"
    }


privacyConfigTests : Test
privacyConfigTests =
    describe "PrivacyConfig.withFallback"
        [ test "an instance that names neither document has the deployment's" <|
            \_ ->
                PrivacyConfig.withFallback deployment PrivacyConfig.default
                    |> Expect.equal deployment
        , test "a document the instance names is its own" <|
            \_ ->
                PrivacyConfig.withFallback deployment
                    { privacyUrl = Just "https://instance.example.com/privacy", termsOfServiceUrl = Nothing }
                    |> Expect.equal
                        { privacyUrl = Just "https://instance.example.com/privacy"
                        , termsOfServiceUrl = Just "https://example.com/terms-of-service"
                        }
        , test "without the deployment's, an instance that names neither has none" <|
            \_ ->
                PrivacyConfig.withFallback PrivacyConfig.default PrivacyConfig.default
                    |> Expect.equal PrivacyConfig.default
        ]
