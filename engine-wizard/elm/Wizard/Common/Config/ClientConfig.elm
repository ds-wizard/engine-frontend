module Wizard.Common.Config.ClientConfig exposing
    ( ClientConfig
    , decoder
    , default
    , defaultAppTitle
    , defaultAppTitleShort
    , defaultPrivacyUrl
    , defaultSupportEmail
    , defaultSupportRepositoryName
    , defaultSupportRepositoryUrl
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Wizard.Common.Config.DashboardWidget as DashboardWidget exposing (DashboardWidget)


type alias ClientConfig =
    { appTitle : String
    , appTitleShort : String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    , dashboard : Dict String (List DashboardWidget)
    , privacyUrl : String
    , customMenuLinks : List CustomMenuLink
    , supportEmail : String
    , supportRepositoryName : String
    , supportRepositoryUrl : String
    }


defaultAppTitle : String
defaultAppTitle =
    "{defaultAppTitle}"


defaultAppTitleShort : String
defaultAppTitleShort =
    "{defaultAppTitleShort}"


defaultPrivacyUrl : String
defaultPrivacyUrl =
    "{defaultPrivacyUrl}"


defaultSupportEmail : String
defaultSupportEmail =
    "{defaultSupportEmail}"


defaultSupportRepositoryName : String
defaultSupportRepositoryName =
    "{defaultSupportRepositoryName}"


defaultSupportRepositoryUrl : String
defaultSupportRepositoryUrl =
    "{defaultSupportRepositoryUrl}"


decoder : Decoder ClientConfig
decoder =
    D.succeed ClientConfig
        |> D.optional "appTitle" D.string defaultAppTitle
        |> D.optional "appTitleShort" D.string defaultAppTitleShort
        |> D.optional "welcomeInfo" (D.maybe D.string) Nothing
        |> D.optional "welcomeWarning" (D.maybe D.string) Nothing
        |> D.optional "loginInfo" (D.maybe D.string) Nothing
        |> D.optional "dashboard" DashboardWidget.dictDecoder Dict.empty
        |> D.optional "privacyUrl" D.string defaultPrivacyUrl
        |> D.optional "customMenuLinks" (D.list CustomMenuLink.decoder) []
        |> D.optional "supportEmail" D.string defaultSupportEmail
        |> D.optional "supportRepositoryName" D.string defaultSupportRepositoryName
        |> D.optional "supportRepositoryUrl" D.string defaultSupportRepositoryUrl


default : ClientConfig
default =
    { appTitle = ""
    , appTitleShort = ""
    , welcomeInfo = Nothing
    , welcomeWarning = Nothing
    , loginInfo = Nothing
    , dashboard = Dict.empty
    , privacyUrl = defaultPrivacyUrl
    , customMenuLinks = []
    , supportEmail = defaultSupportEmail
    , supportRepositoryName = defaultSupportRepositoryName
    , supportRepositoryUrl = defaultSupportRepositoryUrl
    }
